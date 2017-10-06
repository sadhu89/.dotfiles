#[[ -s "$HOME/.profile" ]] && source "$HOME/.profile" # Load the default .profile

#[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*


# Swap capslock and ctrl right

# xmodmap -e "remove Lock = Caps_Lock"
# xmodmap -e "remove Control = Control_L"
# xmodmap -e "keysym Control_L = Caps_Lock"
# xmodmap -e "keysym Caps_Lock = Control_L"
# xmodmap -e "add Lock = Caps_Lock"
# xmodmap -e "add Control = Control_L"


# Remap return to ctrl left

# xmodmap -e "remove Control = Control_R"
# xmodmap -e "keycode 0x69 = Return"
# xmodmap -e "keycode 0x24 = Control_R"
# xmodmap -e "add Control = Control_R"
#
# xcape -t 10000 -e "Control_R=Return"

# Swap capslock and ctrl xmodmap

# xmodmap -e "remove Lock = Caps_Lock"
# xmodmap -e "remove Control = Control_L"
# xmodmap -e "keysym Control_L = Scroll_Lock"
# xmodmap -e "keysym Caps_Lock = Control_L"
# xmodmap -e "add Control = Control_L"
