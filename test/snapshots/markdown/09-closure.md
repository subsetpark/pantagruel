### Domains

`Block`

### Rules

**parent** *b*: `Block` ⇒ `Block` + `Nothing`.

**ancestor** *b*: `Block` ⇒ [`Block`] = closure **parent**.

---

∀ *b*: `Block` · ¬*b* ∈ **ancestor** *b*.

