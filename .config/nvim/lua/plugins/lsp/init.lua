return {
  {
    "L3MON4D3/LuaSnip",
    dependencies = {
      "rafamadriz/friendly-snippets",
      config = function()
        require("luasnip.loaders.from_vscode").lazy_load({ paths = { "./snippets" } })
      end,
    },
    opts = {
      history = true,
      delete_check_events = "TextChanged",
    },
    keys = {},
  },

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
    "WhoIsSethDaniel/toggle-lsp-diagnostics.nvim",
    opts = { signs = false },
    config = function(_, opts)
      require("toggle_lsp_diagnostics").init(opts)
    end,
  },

  -- lsp servers
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      { "folke/neodev.nvim" },
      "mason.nvim",
      "jayp0521/mason-nvim-dap.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp",
      "WhoIsSethDaniel/toggle-lsp-diagnostics.nvim",
      { "j-hui/fidget.nvim", config = true, tag = "legacy" },
    },
    opts = {
      servers = {
        taplo = {},
        bashls = {},
        clangd = {
          capabilities = {
            offsetEncoding = { "utf-16" },
          },
        },
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
      require("neodev").setup()
      require("plugins.lsp.util").on_attach(function(client, buffer)
        require("plugins.lsp.keymaps").on_attach(client, buffer)
        require("plugins.lsp.format").on_attach(client, buffer)
      end)
      local servers = opts.servers
      local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

      require("mason").setup()
      require("mason-nvim-dap").setup({
        ensure_installed = {},
        automatic_installation = false,
      })
      local ensure_installed = vim.tbl_keys(servers)
      require("mason-lspconfig").setup({ ensure_installed = ensure_installed })
      require("mason-lspconfig").setup_handlers({
        function(server)
          local server_opts = servers[server] or {}
          local initial_capabilities = server_opts.capabilities
          server_opts.capabilities = vim.tbl_deep_extend("keep", capabilities, initial_capabilities or {})
          if opts.setup[server] then
            if opts.setup[server](server, server_opts) then
              return
            end
          elseif opts.setup["*"] then
            if opts.setup["*"](server, server_opts) then
              return
            end
          end
          require("lspconfig")[server].setup(server_opts)
        end,
      })
      require("toggle_lsp_diagnostics").init()
    end,
  },

  -- null-ls
  {
    "jose-elias-alvarez/null-ls.nvim",
    event = "VeryLazy",
    config = function()
      local nls = require("null-ls")
      nls.setup({
        debounce = 150,
        save_after_format = false,
        sources = {
          nls.builtins.formatting.prettierd,
          -- nls.builtins.formatting.stylua,
          nls.builtins.diagnostics.shellcheck,
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
          nls.builtins.diagnostics.flake8,
        },
        root_dir = require("null-ls.utils").root_pattern(".git"),
      })
    end,
  },
}
