# keep only the first occurrence of each duplicate entry in ~/.bash_history
#
# 1. Break records at points matching a timestap. Special case
# for the first and last lines in the history file (setting RS to a regexp is a
# GNU awk extension). This way the script matches multi-line commands in the file
# 2. Set the timestamp of the current record to the previous record separator,
# trim starting line break
# 3. Skip empty lines (e.g. the one produced while processing the first line)

out=$(mktemp)
gawk -vRS='(^|\n)#[0-9]+\n' \
    '{
        sub(/\n$/, "")
        if ($0 && cmds[$0]++ == 0)
            print time $0
        time = RT
        sub(/^\n/, "", time)
    }' < ~/.bash_history > $out
mv $out ~/.bash_history
