# Upgrade all packages by using Homebrew
echo "Start Upgrading by Using Homebrew"
brew update
brew outdated
brew upgrade
echo "Finish Upgrading"

# Clean the useless packages
echo "Start cleaning up"
brew cleanup
echo "Finish Cleaning"
