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
  use 'akinsho/toggleterm.nvim'
  use {
    'magnickolas/vim-markdown',
    requires = {'godlygeek/tabular', opt = true}
  }
  use 'chrisbra/Colorizer'
  use 'tpope/vim-fugitive'
  use 'lervag/vimtex'
  use 'neovim/nvim-lspconfig'
  use 'simrat39/rust-tools.nvim'
  use 'terrortylor/nvim-comment'
  use 'williamboman/nvim-lsp-installer'
  use 'lambdalisue/suda.vim'
  use 'ojroques/vim-oscyank'
  use 'jenterkin/vim-autosource'
  use 'GoldsteinE/compe-latex-symbols'
  use 'honza/vim-snippets'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-vsnip'
  use 'hrsh7th/vim-vsnip'
  use 'AlexeySachkov/llvm-vim'
  use 'nvim-telescope/telescope-fzy-native.nvim'
  use 'dyng/ctrlsf.vim'
  use 'tpope/vim-vinegar'
  use {
    'alvarosevilla95/luatab.nvim',
    requires='kyazdani42/nvim-web-devicons'
  }
  use {
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require("trouble").setup {}
    end
  }
end)
