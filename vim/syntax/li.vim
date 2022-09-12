
syntax region liString start=/\v"/ skip=/\v\\./ end=/\v"/
highligh link liString String

syntax region liChar start=/\v'/ skip=/\v\\./ end=/\v'/
highligh link liChar Character

syn match liNumber /\(\w\)\@<![0-9]\+/
highligh link liNumber Number

syn keyword liKeyword if else fn let alias while break continue return
highligh link liKeyword Keyword

syn keyword liType int string bool pair
highligh link liType Type

syn keyword liBoolean true false 
highligh link liBoolean Boolean

syn keyword liConstant null
highligh link liConstant Constant

syn keyword liBuiltin print println substr char_at string_of_char read_line fst snd
highligh link liBuiltin Function

syn match liOperator "\v:"
syn match liOperator "\v\="
syn match liOperator "\v\+"
syn match liOperator "\v-" 
syn match liOperator "\v\*"
syn match liOperator "\v/"
syn match liOperator "\v\+\="
syn match liOperator "\v-\="
syn match liOperator "\v\*\="
syn match liOperator "\v/\="
syn match liOperator "\v\=\="
syn match liOperator "\v!\="
syn match liOperator "\v\<"
syn match liOperator "\v\>"
syn match liOperator "\v\<\="
syn match liOperator "\v\>\="
syn match liOperator "\v\=\=\="
syn match liOperator "\v!\=\="
syn match liOperator "\v\$"
syn match liOperator "\v\=\>"
highligh link liOperator Operator

syntax match liComment "\v//.*$"
highligh link liComment Comment
