const { Plugin, Modal, Notice } = require("obsidian");
const { spawn } = require("child_process");
const windows = process.platform === "win32";

// Asks for one value, like the <url> in "bb decklist <url>".
class Prompt extends Modal {
  constructor(app, label, onSubmit) { super(app); this.label = label; this.onSubmit = onSubmit; }
  onOpen() {
    this.contentEl.createEl("h3", { text: this.label });
    const input = this.contentEl.createEl("input", { type: "text", attr: { style: "width: 100%" } });
    input.addEventListener("keydown", (e) => { if (e.key === "Enter") { this.close(); this.onSubmit(input.value.trim()); } });
    input.focus();
  }
  onClose() { this.contentEl.empty(); }
}

module.exports = class extends Plugin {
  onload() {
    this.running = {};
    this.bb("tasks", (out) => {
      const tasks = [...out.matchAll(/^(\S+)\s{2,}(.*)$/gm)];
      if (!tasks.length) return new Notice("bb tasks found nothing:\n" + out, 10000);
      for (const [, name, doc] of tasks) {
        const arg = doc.match(/<(\w+)>/)?.[1];
        this.addCommand({ id: name, name: `${name} (${doc})`, callback: () =>
          arg ? new Prompt(this.app, `bb ${name} <${arg}>`, (value) => value && this.run(name, `"${value.replace(/"/g, "")}"`)).open()
              : this.run(name) });
      }
    });
  }

  // Runs bb through a shell that knows where it is: Obsidian's own PATH on a Mac does not.
  bb(task, done) {
    const [shell, args] = windows ? ["cmd.exe", ["/c", `bb ${task}`]] : [process.env.SHELL || "/bin/zsh", ["-lc", `bb ${task}`]];
    const proc = spawn(shell, args, { cwd: this.app.vault.adapter.basePath, detached: !windows, windowsHide: true });
    let out = "";
    proc.stdout.on("data", (d) => (out += d));
    proc.stderr.on("data", (d) => (out += d));
    proc.on("exit", () => done(out.trim()));
    return proc;
  }

  run(name, arg = "") {
    this.stop(name);
    this.running[name] = this.bb(`${name} ${arg}`, (out) => new Notice(`bb ${name}\n${out.split("\n").slice(-6).join("\n")}`, 10000));
    new Notice(`bb ${name} started`, 3000);
  }

  // Kills the task and everything it started: the process group here, the process tree on Windows.
  stop(name) {
    const proc = this.running[name];
    if (!proc) return;
    try {
      if (windows) spawn("taskkill", ["/pid", String(proc.pid), "/t", "/f"], { windowsHide: true });
      else process.kill(-proc.pid);
    } catch {}
  }

  onunload() { Object.keys(this.running).forEach((name) => this.stop(name)); }
};
