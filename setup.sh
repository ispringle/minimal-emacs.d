#!/usr/bin/env bash
# setup.sh - Setup script for minimal-emacs.d configuration

set -e

EMACS_DIR="$HOME/.emacs.d"
CONFIG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Setting up minimal-emacs.d..."

# Clone the minimal-emacs.d repository if it doesn't exist
if [ ! -d "$EMACS_DIR" ]; then
    echo "Cloning minimal-emacs.d repository to $EMACS_DIR..."
    git clone --depth 1 https://github.com/jamescherti/minimal-emacs.d "$EMACS_DIR"
else
    echo "$EMACS_DIR already exists, skipping clone."
fi

# Create symlink for pre-early-init.el
if [ -f "$CONFIG_DIR/pre-early-init.el" ]; then
    echo "Creating symlink for pre-early-init.el..."
    ln -sf "$CONFIG_DIR/pre-early-init.el" "$EMACS_DIR/pre-early-init.el"
    echo "Symlink created: $EMACS_DIR/pre-early-init.el -> $CONFIG_DIR/pre-early-init.el"
else
    echo "Warning: pre-early-init.el not found in $CONFIG_DIR"
fi

echo "Setup complete!"
echo "Your Emacs configuration is ready at $EMACS_DIR"
