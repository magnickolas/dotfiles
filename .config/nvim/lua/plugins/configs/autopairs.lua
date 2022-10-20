local autopairs = require('nvim-autopairs')
autopairs.setup{
    map_bs = false,
}
autopairs.remove_rule('(')
autopairs.remove_rule('\'')
autopairs.remove_rule('"')
autopairs.remove_rule('[')
