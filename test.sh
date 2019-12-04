results=(
    3464735
    5194211
    4330636
    6086
    232
    6084
    1716
    1163
)

test() {
    day=$(( $1 / 2 + 1 ))
    echo "== Day $day =="

    echo "part 1"
    val=$(cat inputs/day$day.txt | stack run $day 1)
    res=${results[$1 + 1]}
    if [[ $val = $res ]]; then
        echo "OK"
    else
        echo "KO"
    fi

    echo "part 2"
    val=$(cat inputs/day$day.txt | stack run $day 2)
    res=${results[$1 + 2]}
    if [[ $val = $res ]]; then
        echo "OK"
    else
        echo "KO"
    fi
}

i=0
while [[ $i < ${#results[@]} ]]; do 
    test $i
    i=$(( $i + 2 ))
done

#cat inputs/day1.txt | stack run 1 2 == 5194211