local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  use 'tpope/vim-sensible'

  use 'folke/tokyonight.nvim'
  use 'tanvirtin/monokai.nvim'

  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true }
  }
  use {
    "lewis6991/gitsigns.nvim",
    requires = {
      "nvim-lua/plenary.nvim"
    },
    config = function()
      require("gitsigns").setup()
    end
  }

  use {'akinsho/bufferline.nvim', requires = 'kyazdani42/nvim-web-devicons'}
  use 'farmergreg/vim-lastplace'
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }
  use 'github/copilot.vim'
  use {'neovim/nvim-lspconfig', 'williamboman/nvim-lsp-installer'}

  use 'nvim-lua/popup.nvim'
  use 'nvim-lua/plenary.nvim'
  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  use {
    'kyazdani42/nvim-tree.lua',
    requires = {
      'kyazdani42/nvim-web-devicons',
    }
  }

  use 'google/vim-maktaba'
  use 'google/vim-glaive'
  use 'google/vim-codefmt'

  if packer_bootstrap then
    require('packer').sync()
  end
end)

-- settings
vim.g.encoding = "UTF-8"
vim.o.fileencoding = 'utf-8'
vim.o.scrolloff = 8
vim.o.sidescrolloff = 8
vim.wo.number = true
vim.wo.relativenumber = false
vim.wo.cursorline = true
vim.wo.signcolumn = "yes"
vim.wo.colorcolumn = "120"
vim.o.tabstop = 2
vim.bo.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftround = true
vim.o.shiftwidth = 2
vim.bo.shiftwidth = 2
vim.o.expandtab = true
vim.bo.expandtab = true
vim.o.autoindent = true
vim.bo.autoindent = true
vim.o.smartindent = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.hlsearch = false
vim.o.incsearch = true
vim.o.showmode = false
vim.o.cmdheight = 2
vim.o.autoread = true
vim.bo.autoread = true
vim.o.wrap = false
vim.wo.wrap = false
vim.o.whichwrap = 'b,s,<,>,[,],h,l'
vim.o.hidden = true
vim.o.mouse = "a"
vim.o.backup = false
vim.o.writebackup = false
vim.o.swapfile = false
vim.o.updatetime = 300
vim.o.timeoutlen = 100
vim.o.splitbelow = true
vim.o.splitright = true
vim.g.completeopt = "menu,menuone,noselect,noinsert"
vim.o.background = "dark"
vim.o.termguicolors = true
vim.opt.termguicolors = true
vim.o.list = true
vim.o.listchars = "space:·"
vim.o.wildmenu = true
vim.o.shortmess = vim.o.shortmess .. 'c'
vim.o.pumheight = 10
vim.o.showtabline = 2
vim.o.breakindent = true
vim.opt.termguicolors = true
vim.opt.undofile = true
vim.opt.clipboard = "unnamedplus"


-- colorscheme
vim.g.tokyonight_style = "storm"
vim.g.tokyonight_italic_functions = true
vim.g.tokyonight_sidebars = { "qf", "vista_kind", "terminal", "packer" }
vim.g.tokyonight_colors = { hint = "orange", error = "#ff0000" }

-- vim.cmd [[colorscheme tokyonight]]
vim.cmd [[colorscheme monokai_pro]]

require('monokai').setup { palette = require('monokai').pro }

require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'tokyonight',
    component_separators = '|',
    section_separators = '',
  },
}

require("gitsigns").setup(
  {
    signs = {
      add          = {hl = 'GitSignsAdd'   , text = '│', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
      change       = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
      delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
      topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
      changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    },
  }
)

require("bufferline").setup(
  {
    options = {
      numbers = "ordinal",
      diagnostics = "nvim_lsp",
      indicator_icon = "▎",
      buffer_close_icon = "",
      modified_icon = "●",
      close_icon = "",
      left_trunc_marker = "",
      right_trunc_marker = "",
      separator_style = "thin",
      offsets = {
        {
          filetype = "NvimTree",
          text = "File Explorer",
          highlight = "Directory",
          text_align = "left"
        }
      },
      diagnostics_indicator = function(count, level, diagnostics_dict, context)
        local s = " "
        for e, n in pairs(diagnostics_dict) do
          local sym = e == "error" and " " or (e == "warning" and " " or "")
          s = s .. n .. sym
        end
        return s
      end
    }
  }
)

require'nvim-treesitter.configs'.setup {
  ensure_installed = {"c", "cpp", "python", "html", "css", "vim", "lua", "javascript", "typescript", "tsx"},
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<CR>',
      node_incremental = '<CR>',
      node_decremental = '<BS>',
      scope_incremental = '<TAB>',
    }
  },
  indent = {
    enable = true
  }
}

vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'
vim.wo.foldlevel = 99

require('nvim-tree').setup {
  auto_reload_on_write = true,
  disable_netrw = false,
  hide_root_folder = false,
  sort_by = "name",
  git = {
    enable = true,
    ignore = true,
    timeout = 400,
  },
}


-- keybindings
local opts = { noremap = true, silent = true }
local keymap = vim.api.nvim_set_keymap
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal --
-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)
