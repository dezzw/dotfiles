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
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
brew install autojump
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions


# Set up .zshrc file
rm ~/.zshrc
ln -s ~/Documents/dotfiles/.zshrc ~/.zshrc
echo "Finsh Installing Zsh and Oh-may-zsh"

# install some fonts
brew tap homebrew/cask-fonts
brew install --cask font-hack-nerd-font

# Install Emacs
echo "Start Installing Emacs-plus"
brew reinstall gcc libgccjit
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-xwidgets --with-native-comp
rm -rf ~/.emacs.d
ln -s ~/Documents/dotfiles/.emacs.d ~/.emacs.d
echo "Finish Installing Emacs-plus"

# Install Python
echo "Start Installing Python"
brew install pyenv
brew install pyenv-virtualenv
echo "Start Installing Python"

# Install node and packages
echo "Start Installing node and packages"
brew install nvm
nvm install node
echo "Finsh Installing node and packages"
