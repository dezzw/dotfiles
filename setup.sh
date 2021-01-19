# Install Command Line Tool
echo "Start Installing Command Line Tool"
xcode-select --install
echo "Finish Installing Command Line Tool"

# Install Homebrew
echo "Start Installing Homebrew"
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
echo "Finish Installing Homebrew"

# Install New version of Zsh
echo "Start Installing Zsh and Oh-my-zsh"
brew install zsh
brew install zsh-autosuggestions
brew install autojump
brew install zsh-syntax-highlighting

# Set up .zshrc file
rm ~/.zshrc
ln -s ~/Documents/dotfiles/.zshrc ~/.zshrc
echo "Finsh Installing Zsh and Oh-may-zsh"

# Install Emacs
echo "Start Installing Emacs-plus"
brew tap d12frosted/emacs-plus
brew install emacs-plus --with-xwidgets
rm -rf ~/.emacs.d
ln -s ~/Documents/dotfiles/.emacs.d ~/.emacs.d
echo "Finish Installing Emacs-plus"

# Install Python
echo "Start Installing Python"
brew install python
echo "Start Installing Python"

# Install node and packages
echo "Start Installing node and packages"
brew install node
echo "Finsh Installing node and packages"
