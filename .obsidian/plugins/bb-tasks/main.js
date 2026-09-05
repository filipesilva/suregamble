const { Plugin, Notice } = require("obsidian");
const { execFile, spawn } = require("child_process");
const BB = "/opt/homebrew/bin/bb";

module.exports = class extends Plugin {
  onload() {
    const cwd = this.app.vault.adapter.basePath;
    this.addCommand({ id: "build", name: "bb build", callback: () =>
      execFile(BB, ["build"], { cwd }, (err, out, errOut) => new Notice(out || errOut, 8000)) });
    this.addCommand({ id: "dev", name: "bb dev (serve and reload on save)", callback: () => {
      this.dev?.kill();
      this.dev = spawn(BB, ["dev"], { cwd, stdio: "ignore" });
      new Notice("bb dev: http://localhost:8080/catalyst/", 8000);
    } });
  }
  onunload() { this.dev?.kill(); }
};
