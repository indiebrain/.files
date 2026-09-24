-- Personal Hyprland bindings, loaded after Omarchy's defaults.
-- Part of the dotfiles hypr stow package; stowed only on Omarchy.
--
-- See current bindings:  omarchy menu keybindings --print
-- Add:      o.bind(keys, description, dispatcher)
-- Replace:  o.rebind(keys, description, dispatcher)
-- Remove:   hl.unbind(keys)
--
-- Super+Shift+N already opens the default editor (set to Emacs by the
-- defaults module). This adds a quick emacsclient frame on the daemon.
o.bind("SUPER + ALT + E", "Emacs frame", { launch = "emacsclient -c -a ''" })

-- Firefox Developer Edition's window class (firefoxdeveloperedition) isn't in
-- Omarchy's Firefox rule, so tag it to get the same full-opacity treatment.
o.window("firefoxdeveloperedition", { tag = "+firefox-based-browser" })
