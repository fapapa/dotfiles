#!/bin/bash

set -e

EMAIL="fpapa@springhealth.com"
KEY_PATH="$HOME/.ssh/id_ed25519"

if [[ -f "$KEY_PATH" ]]; then
  echo "🔑 SSH key already exists at $KEY_PATH"
else
  echo "🔐 Generating SSH key..."
  ssh-keygen -t ed25519 -C "$EMAIL" -f "$KEY_PATH" -N ""
  eval "$(ssh-agent -s)"
  ssh-add "$KEY_PATH"
  pbcopy <"${KEY_PATH}.pub"
  echo "✅ SSH key generated and copied to clipboard."

  echo
  echo "👉 Add this key to GitHub: https://github.com/settings/ssh/new"
  echo "🧪 Then test: ssh -T git@github.com"
fi
