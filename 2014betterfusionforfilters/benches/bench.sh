function get_time {
    local cmd="$*"
    s=` { time -p $cmd 1>/dev/null ; } 2>&1 | grep real | awk '{print $2}'`
    t_out=$s
}


glob=$1
sz=$2
for x in out/$glob*
do
    echo -n $x " "
    get_time $x $sz
    echo -n $t_out " "
    get_time $x $sz
    echo -n $t_out " "
    get_time $x $sz
    echo -n $t_out " "
    get_time $x $sz
    echo -n $t_out " "
    get_time $x $sz
    echo -n $t_out " "
    echo
done
