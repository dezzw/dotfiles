{ pkgs, ... }:
with pkgs;
let
  my-python-packages = python38Packages: with python38Packages; [
    # for eaf
    pyqt5
    pyqtwebengine
    sip qrcode
    epc
    retry

    # eaf-filemanager
    lxml

    # eaf-system-monitor
    psutil

    # other
    pip
    qtawesome
    percol
  ];
  python-with-my-packages = python38.withPackages my-python-packages;
in
python-with-my-packages
