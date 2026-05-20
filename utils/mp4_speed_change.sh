mp4_speed_change() {
    # 引数の数を確認
    if [ "$#" -ne 3 ]; then
        echo "Usage: vspeed <input_file> <output_file> <speed>"
        return 1
    fi

    local INPUT_FILE="$1"
    local OUTPUT_FILE="$2"
    local SPEED="$3"

    # 再生速度の検証
    if (( $(echo "$SPEED <= 0" | bc -l) )); then
        echo "Error: Speed must be greater than 0."
        return 1
    fi

    # 音声フィルタの設定 (速度が2倍を超える場合は連続適用)
    local AUDIO_FILTER=""
    if (( $(echo "$SPEED > 2" | bc -l) )); then
        local TEMP_SPEED=$SPEED
        while (( $(echo "$TEMP_SPEED > 2" | bc -l) )); do
            AUDIO_FILTER+="atempo=2.0,"
            TEMP_SPEED=$(echo "$TEMP_SPEED / 2" | bc -l)
        done
        AUDIO_FILTER+="atempo=$TEMP_SPEED"
    else
        AUDIO_FILTER="atempo=$SPEED"
    fi

    # ffmpegコマンドを実行
    echo "Processing video..."
    ffmpeg -i "$INPUT_FILE" -filter:v "setpts=PTS/$SPEED" -filter:a "$AUDIO_FILTER" "$OUTPUT_FILE"

    if [ $? -eq 0 ]; then
        echo "Successfully processed $INPUT_FILE to $OUTPUT_FILE with $SPEEDx speed."
    else
        echo "Failed to process video."
        return 1
    fi
}
