
syntax region liString start=/\v"/ skip=/\v\\./ end=/\v"/
highligh link liString String

syn match liNumber /\(\w\)\@<![0-9]\+/
highligh link liNumber Number

syn keyword liKeyword if else fn __return let
highligh link liKeyword Keyword

syn keyword liBoolean true false
highligh link liBoolean Boolean

syn keyword liBuiltin print println substr char_at 
highligh link liBuiltin Function

syn match liOperator "\v:\="
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
