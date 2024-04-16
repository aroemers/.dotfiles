## Terminal integration

For macOS Terminal to always start and continue in tmux, use the following as "Shells open with:" in Terminal's General preferences.

```
/opt/homebrew/bin/tmux new-session -A
```
