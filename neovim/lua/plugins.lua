require('packer').startup(function()
  use 'wbthomason/packer.nvim'
  use 'ludovicchabant/vim-gutentags'
  use 'Mofiqul/vscode.nvim'
  use 'beyondmarc/glsl.vim'
  use 'mg979/vim-visual-multi'
  use 'simnalamburt/vim-mundo'
  use 'hyiltiz/vim-plugins-profile'
  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }
  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }
  use 'gosukiwi/vim-atom-dark'
  use 'arcticicestudio/nord-vim'
  use 'junegunn/vim-easy-align'
  use 'romgrk/barbar.nvim'
  use 'akinsho/toggleterm.nvim'
  use {
    'magnickolas/vim-markdown',
    requires = {'godlygeek/tabular', opt = true}
  }
  use 'chrisbra/Colorizer'
  use 'tpope/vim-fugitive'
  use 'lervag/vimtex'
  use 'neovim/nvim-lspconfig'
  use 'hrsh7th/nvim-compe'
  use 'simrat39/rust-tools.nvim'
  use 'terrortylor/nvim-comment'
  use 'hrsh7th/vim-vsnip'
  use 'kabouzeid/nvim-lspinstall'
  use 'lambdalisue/suda.vim'
  use 'ojroques/vim-oscyank'
end)
