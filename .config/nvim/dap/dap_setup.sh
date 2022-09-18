#!/bin/bash
# cpp-tools
wget https://github.com/microsoft/vscode-cpptools/releases/download/v1.12.4/cpptools-linux.vsix 
unzip -d cpptools cpptools-linux.vsix
chmod +x cpptools/extension/debugAdapters/bin/OpenDebugAD7
rm cpptools-linux.vsix

# codelldb
wget https://github.com/vadimcn/vscode-lldb/releases/download/v1.7.4/codelldb-x86_64-linux.vsix
unzip -d codelldb codelldb-x86_64-linux.vsix
chmod +x codelldb/extension/adapter/codelldb
rm codelldb-x86_64-linux.vsix
