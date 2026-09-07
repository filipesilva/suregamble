# Sure Gamble

This repo works as an [Obsidian vault](https://obsidian.md) that builds a website using [Babashka](https://book.babashka.org) automations over [Markdown Properties](https://obsidian.md/help/properties).

Install both Obsidian and Babashka, then open the vault in Obsidian to write content, or in a code editor to edit the automation.


## Structure

```
.
├── articles/        one md per article
├── authors/         one md per author
├── series/          one md per series
├── pages/           hq.md is the front page, 404.md the not found page, the rest become <slug>/
├── decklists/       one md per NetrunnerDB decklist, written by bb decklist
├── cards/           one md per NetrunnerDB card, written by bb cards
├── bases/           Obsidian bases for vault overview
├── components/      HTML and css fragments
├── assets/          site.css and images, copied to public/assets/
├── src/             babashka code
├── public/          the generated site
└── bb.edn           babashka tasks
```


## Babashka Tasks

These tasks can be ran from the CLI or from within Obsidian:
- `bb build` writes the published site into `public/`
- `bb dev` serves it at http://localhost:8080 with drafts included, rebuilds on save and reloads the browser
- `bb cards` fetches every NetrunnerDB card into `cards/` as notes, so `[[Sure Gamble]]` links and previews in Obsidian and on the site
- `bb decklist <url>` fetches a decklist into `decklists/`, ready to embed in an article with `![[its name]]`.
