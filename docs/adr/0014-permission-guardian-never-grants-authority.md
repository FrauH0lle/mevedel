# Permission guardian never grants authority

The permission guardian supplies model-based semantic risk guidance after
deterministic Bash analysis and permission policy have produced an ask.  It is
advisory in `ask` and `auto`, may only veto Bash in `full-auto`, and never
promotes an ask to allow or changes the deterministic command category.  This
keeps model judgment useful in the prompt without making a second model an
authority source; unavailable guidance leaves interactive prompts intact and
does not defeat the deliberately unattended `full-auto` mode. Its positive
recommendation is named `proceed`, not `allow-once`, so model advice cannot be
mistaken for permission authority.
