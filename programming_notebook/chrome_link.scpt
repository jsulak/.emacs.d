tell application "Google Chrome"
	set t to title of active tab of window 1
	set U to URL of active tab of window 1
	-- set the clipboard to "[" & t & "](" & U & ")"
	return "[" & t & "](" & U & ")"
end tell
