vim.g.gutentags_cache_dir = vim.fn.expand("~/.cache/vim/ctags/")
vim.g.gutentags_generate_on_write = 1
vim.g.gutentags_ctags_extra_args = {
   "--tag-relative=yes",
   "--fields=+ailmnS"
}
vim.g.gutentags_ctags_exclude = {
  "*.git", "*.svg", "*.hg",
  "*/tests/*",
  "build",
  "dist",
  "*sites/*/files/*",
  "bin",
  "node_modules",
  "bower_components",
  "cache",
  "compiled",
  "docs",
  "example",
  "bundle",
  "vendor",
  "*.md",
  "*-lock.json",
  "*.lock",
  "*bundle*.js",
  "*build*.js",
  ".*rc*",
  "*.json",
  "*.min.*",
  "*.map",
  "*.bak",
  "*.zip",
  "*.pyc",
  "*.class",
  "*.sln",
  "*.Master",
  "*.csproj",
  "*.tmp",
  "*.csproj.user",
  "*.cache",
  "*.pdb",
  "tags*",
  "cscope.*",
  "*.css",
  "*.less",
  "*.scss",
  "*.exe", "*.dll",
  "*.mp3", "*.ogg", "*.flac",
  "*.swp", "*.swo",
  "*.bmp", "*.gif", "*.ico", "*.jpg", "*.png",
  "*.rar", "*.zip", "*.tar", "*.tar.gz", "*.tar.xz", "*.tar.bz2",
  "*.pdf", "*.doc", "*.docx", "*.ppt", "*.pptx",
}

