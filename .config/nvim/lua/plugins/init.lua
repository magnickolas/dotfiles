require('packer').startup(function()
    use 'wbthomason/packer.nvim'
    use {
        'ludovicchabant/vim-gutentags',
        config = function()
            require('plugins.configs.gutentags')
        end
    }
    use 'Mofiqul/vscode.nvim'
    use 'beyondmarc/glsl.vim'
    use 'mg979/vim-visual-multi'
    use 'simnalamburt/vim-mundo'
    use 'hyiltiz/vim-plugins-profile'
    use {
        'nvim-telescope/telescope.nvim',
        requires = { { 'nvim-lua/plenary.nvim' } },
        config = function()
            require('plugins.configs.telescope')
        end
    }
    use {
        'hoob3rt/lualine.nvim',
        requires = {
            { 'kyazdani42/nvim-web-devicons' },
            { 'nvim-lua/plenary.nvim' }
        },
        config = function()
            require('plugins.configs.lualine')
        end
    }
    use 'gosukiwi/vim-atom-dark'
    use 'arcticicestudio/nord-vim'
    use 'junegunn/vim-easy-align'
    use {
        'NvChad/nvterm',
        config = function()
            require('plugins.configs.nvterm')
        end
    }
    use {
        'magnickolas/vim-markdown',
        requires = { 'godlygeek/tabular', opt = true }
    }
    use 'chrisbra/Colorizer'
    use 'tpope/vim-fugitive'
    use 'lervag/vimtex'
    use {
        'neovim/nvim-lspconfig',
        config = function()
            require('plugins.configs.lspconfig')
        end
    }
    use {
        'simrat39/rust-tools.nvim',
        config = function()
            require('plugins.configs.rust-tools')
        end
    }
    use {
        'terrortylor/nvim-comment',
        config = function()
            require('plugins.configs.comment')
        end
    }
    use {
        'williamboman/nvim-lsp-installer',
        config = function()
            require('plugins.configs.lsp-installer')
        end
    }
    use 'lambdalisue/suda.vim'
    use {
        'ojroques/vim-oscyank',
        config = function()
            require('plugins.configs.oscyank')
        end
    }
    use {
        'jenterkin/vim-autosource',
        config = function()
            require('plugins.configs.autosource')
        end
    }
    use 'GoldsteinE/compe-latex-symbols'
    use {
        'hrsh7th/nvim-cmp',
        config = function()
            require('plugins.configs.cmp')
        end
    }
    use {
        'L3MON4D3/LuaSnip',
        config = function()
            require('plugins.configs.luasnip')
        end
    }
    use 'saadparwaiz1/cmp_luasnip'
    use 'hrsh7th/cmp-nvim-lua'
    use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-path'
    use 'AlexeySachkov/llvm-vim'
    use 'dyng/ctrlsf.vim'
    use 'tpope/vim-vinegar'
    use {
        'alvarosevilla95/luatab.nvim',
        requires = 'kyazdani42/nvim-web-devicons',
        config = function()
            require('plugins.configs.luatab')
        end
    }
    use {
        'folke/trouble.nvim',
        requires = 'kyazdani42/nvim-web-devicons',
        config = function()
            require('trouble').setup {}
        end
    }
    use 'chriskempson/base16-vim'
    use 'github/copilot.vim'
    use {
        'kyazdani42/nvim-tree.lua',
        requires = {
            'kyazdani42/nvim-web-devicons',
        },
        config = function()
            require('plugins.configs.nvim-tree')
        end
    }
    use {
        'windwp/nvim-autopairs',
        config = function()
            require('plugins.configs.autopairs')
        end
    }
end)
