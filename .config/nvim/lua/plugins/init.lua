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
        requires = {
            'nvim-lua/plenary.nvim',
            'nvim-telescope/telescope-smart-history.nvim',
            'kkharji/sqlite.lua',
        },
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
        end,
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
    use 'hrsh7th/cmp-cmdline'
    use 'hrsh7th/cmp-path'
    use 'AlexeySachkov/llvm-vim'
    use 'dyng/ctrlsf.vim'
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
            require('plugins.configs.trouble')
        end,
    }
    use 'chriskempson/base16-vim'
    use 'github/copilot.vim'
    use {
        'windwp/nvim-autopairs',
        config = function()
            require('plugins.configs.autopairs')
        end,
    }
    use 'alfredodeza/pytest.vim'
    use {
        'saecki/crates.nvim',
        requires = 'nvim-lua/plenary.nvim',
        config = function()
            require('crates').setup()
        end,
    }
    -- use {
    --     '~/arcadia/junk/magnickolas/arcblamer.nvim',
    --     config = function()
    --         vim.g.arcblamer_relative_time = 1
    --         vim.g.arcblamer_template = '<commit-short> <author> <author-time> • <summary>'
    --     end,
    -- }
    use 'sakhnik/nvim-gdb'
    use 'nvim-telescope/telescope-file-browser.nvim'
    use 'folke/tokyonight.nvim'
    use {'kevinhwang91/nvim-bqf', ft='qf'}
    use {'junegunn/fzf', run = function() vim.fn['fzf#install']() end }
    use 'junegunn/fzf.vim'
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        config = function()
            require "nvim-treesitter.configs".setup {
              query_linter = {
                enable = true,
                use_virtual_text = true,
                lint_events = {"BufWrite", "CursorHold"},
              },
            }
        end,
    }
    use {
        'nvim-treesitter/playground',
        requires='nvim-treesitter/nvim-treesitter',
        run = ':TSInstall query',
    }
    use 'tpope/vim-dispatch'
    use {
        'folke/which-key.nvim',
        config = function()
            require("which-key").setup {}
        end
    }
    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    }
    use {
        'mfussenegger/nvim-dap',
        config = function()
            require('plugins.configs.dap')
        end,
    }
    use {
        'rcarriga/nvim-dap-ui',
        requires = {'mfussenegger/nvim-dap'},
        config = function()
            require('dapui').setup()
        end,
    }
    use {
        'theHamsta/nvim-dap-virtual-text',
        config = function()
            require('nvim-dap-virtual-text').setup()
        end,
    }
end)
