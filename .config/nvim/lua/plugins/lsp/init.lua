return {
  -- {
  --     "L3MON4D3/LuaSnip",
  --     dependencies = {
  --         "rafamadriz/friendly-snippets",
  --         config = function()
  --             require("luasnip.loaders.from_vscode").lazy_load({ paths = { "./snippets" } })
  --         end,
  --     },
  --     opts = {
  --         history = true,
  --         delete_check_events = "TextChanged",
  --     },
  --     keys = {},
  -- },

  -- tools
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        "stylua",
        "luacheck",
        "eslint_d",
        "shellcheck",
        "deno",
        "shfmt",
        "black",
        "isort",
        "flake8",
      },
    },
  },

  -- json schemas
  "b0o/SchemaStore.nvim",

  {
    "folke/lazydev.nvim",
    ft = "lua",
  },

  -- lsp servers
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "saghen/blink.cmp",
      "mason.nvim",
      "jayp0521/mason-nvim-dap.nvim",
      "williamboman/mason-lspconfig.nvim",
      { "j-hui/fidget.nvim", config = true, tag = "legacy" },
    },
    opts = {
      servers = {
        taplo = {},
        bashls = {},
        clangd = {},
        dockerls = {},
        jsonls = {
          on_new_config = function(new_config)
            new_config.settings.json.schemas = new_config.settings.json.schemas or {}
            vim.list_extend(new_config.settings.json.schemas, require("schemastore").json.schemas())
          end,
          settings = {
            json = {
              format = {
                enable = true,
              },
              validate = { enable = true },
            },
          },
        },
        lua_ls = {
          settings = {
            Lua = {
              workspace = {
                checkThirdParty = "Disable",
                library = {
                  vim.env.VIMRUNTIME,
                },
              },
            },
          },
        },
        marksman = {},
        pyright = {},
        yamlls = {},
        vimls = {},
      },
      setup = {},
    },
    config = function(_, opts)
      local lspconfig = require("lspconfig")
      require("plugins.lsp.util").on_attach(function(client, buffer)
        require("plugins.lsp.keymaps").on_attach(client, buffer)
        require("plugins.lsp.format").on_attach(client, buffer)
      end)
      local servers = opts.servers
      --local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())
      for server, config in pairs(opts.servers) do
        -- passing config.capabilities to blink.cmp merges with the capabilities in your
        -- `opts[server].capabilities, if you've defined it
        config.capabilities = require("blink.cmp").get_lsp_capabilities(config.capabilities)
        lspconfig[server].setup(config)
      end

      require("mason").setup()
      require("mason-nvim-dap").setup({
        ensure_installed = {},
        automatic_installation = false,
      })
      local ensure_installed = vim.tbl_keys(servers)
      require("mason-lspconfig").setup({ ensure_installed = ensure_installed, automatic_enable = true })
      vim.lsp.enable(servers)
    end,
  },

  -- none-ls
  {
    "nvimtools/none-ls.nvim",
    event = "VeryLazy",
    config = function()
      local nls = require("null-ls")
      nls.setup({
        debounce = 150,
        save_after_format = false,
        sources = {
          nls.builtins.formatting.prettierd,
          -- nls.builtins.formatting.stylua,
          -- nls.builtins.diagnostics.shellcheck,
          nls.builtins.formatting.shfmt,
          nls.builtins.formatting.prettierd.with({
            filetypes = { "markdown" },
          }),
          nls.builtins.diagnostics.selene.with({
            condition = function(utils)
              return utils.root_has_file({ "selene.toml" })
            end,
          }),
          nls.builtins.formatting.isort,
          nls.builtins.formatting.black,
          -- nls.builtins.diagnostics.flake8,
        },
        root_dir = require("null-ls.utils").root_pattern(".git"),
      })
    end,
  },
}
