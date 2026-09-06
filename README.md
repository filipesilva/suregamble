This repo works as an obsidian vault that builds a website using babashka automations together with the obsidian cli, using obsidian properties for metadata.

`bb build` writes the published site into `public/`. `bb dev` serves it at http://localhost:8080 with drafts included, rebuilds on save and reloads the browser.

`bb cards` fetches every NetrunnerDB card into `cards/` as notes, so `[[Sure Gamble]]` links and previews in Obsidian and on the site. Rerun it when a set ships. `bb decklist <url>` fetches a decklist into `decklists/`, ready to embed in an article with `![[its name]]`.
