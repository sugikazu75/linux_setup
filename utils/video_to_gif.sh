video_to_gif() {
    local OPTIND opt
    local X=0 Y=0 WIDTH="iw" HEIGHT="ih" START="" END="" FPS=15

    local USAGE="Usage: video_to_gif [-x x] [-y y] [-w width] [-h height] [-s start_time] [-e end_time] [-r fps] <input_file> <output_file>
  -x x           クロップの左上x座標 (default: 0)
  -y y           クロップの左上y座標 (default: 0)
  -w width       クロップの幅 (default: 入力の幅)
  -h height      クロップの高さ (default: 入力の高さ)
  -s start_time  開始時間 (例: 3 や 00:00:03.5) (default: 先頭)
  -e end_time    終了時間 (例: 10 や 00:00:10) (default: 末尾)
  -r fps         gifのフレームレート (default: 15)"

    while getopts "x:y:w:h:s:e:r:" opt; do
        case "$opt" in
            x) X="$OPTARG" ;;
            y) Y="$OPTARG" ;;
            w) WIDTH="$OPTARG" ;;
            h) HEIGHT="$OPTARG" ;;
            s) START="$OPTARG" ;;
            e) END="$OPTARG" ;;
            r) FPS="$OPTARG" ;;
            *) echo "$USAGE"; return 1 ;;
        esac
    done
    shift $((OPTIND - 1))

    if [ "$#" -ne 2 ]; then
        echo "$USAGE"
        return 1
    fi

    local INPUT_FILE="$1"
    local OUTPUT_FILE="$2"

    # 時間指定オプションの組み立て
    local TIME_OPTS=()
    if [ -n "$START" ]; then
        TIME_OPTS+=(-ss "$START")
    fi
    if [ -n "$END" ]; then
        TIME_OPTS+=(-to "$END")
    fi

    # クロップ + パレット生成で高画質gifを作成
    local FILTER="crop=${WIDTH}:${HEIGHT}:${X}:${Y},fps=${FPS},split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse"

    echo "Processing video..."
    ffmpeg "${TIME_OPTS[@]}" -i "$INPUT_FILE" -vf "$FILTER" "$OUTPUT_FILE" -y

    if [ $? -eq 0 ]; then
        echo "Successfully converted $INPUT_FILE to $OUTPUT_FILE."
    else
        echo "Failed to convert video."
        return 1
    fi
}
