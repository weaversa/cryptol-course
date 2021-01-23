wget https://github.com/GaloisInc/cryptol/releases/download/2.10.0/cryptol-2.10.0-Linux-x86_64.tar.gz
tar -xzf cryptol-2.10.0-Linux-x86_64.tar.gz
export PATH=$PATH:$(pwd)/cryptol-2.10.0-Linux-x86_64/bin
wget https://github.com/GaloisInc/saw-script/releases/download/v0.6/saw-0.6-Linux-x86_64.tar.gz
tar -xzf saw-0.6-Linux-x86_64.tar.gz
export PATH=$PATH:$(pwd)/saw-0.6-Linux-x86_64/bin
cryptol --version
saw --version
curl -fsSL https://github.com/Z3Prover/z3/releases/download/z3-4.8.8/z3-4.8.8-x64-ubuntu-16.04.zip -o z3-4.8.8-x64-ubuntu-16.04.zip
unzip -j z3-4.8.8-x64-ubuntu-16.04.zip -d z3_downloaded
export PATH=$PATH:$(pwd)/z3_downloaded
z3 --version
