# CLOCK
showTime = ->
  time = new Date
  hour = time.getHours()
  min = time.getMinutes()
  sec = time.getSeconds()
  am_pm = 'AM'
  if hour > 12
    hour -= 12
    am_pm = 'PM'
  if hour == 0
    hr = 12
    am_pm = 'AM'
  hour = if hour < 10 then '0' + hour else hour
  min = if min < 10 then '0' + min else min
  sec = if sec < 10 then '0' + sec else sec
  currentTime = '<span class="hour-min">' + hour + ':' + min + '</span>' + '<div class="clock-inf"><span class="sec">' + sec + '</span>' + '<span class="am-pm">' + am_pm + '</span></div>'
  document.getElementById('clock').innerHTML = currentTime
  return

setInterval showTime, 1000
  



# CALENDAR

sundayFirstCalendar = 'cal -h && date "+%-m %-d %y"'

mondayFirstCalendar =  'cal -h | awk \'{ print " "$0; getline; print "Mo Tu We Th Fr Sa Su"; \
getline; if (substr($0,1,2) == " 1") print "                    1 "; \
do { prevline=$0; if (getline == 0) exit; print " " \
substr(prevline,4,17) " " substr($0,1,2) " "; } while (1) }\' && date "+%-m %-d %y"'

command: sundayFirstCalendar

#Set this to true to enable previous and next month dates, or false to disable
otherMonths: true

refreshFrequency: 3600000

style: """
  bottom: 2%
  right: 2%
  color: #fff
  color: #77FF00
  
  #clock
    font-weight: 100 
    opacity: 0.6
    font-family: Helvetica
    display: flex
    align-items: center
    gap: 15px
    right: 5px
    position: relative

  .hour-min
    font-size: 70px

  .clock-inf
    display: grid
    font-size: 3.5em
    font-size: 25px
    line-height: 1
    text-align: right

  table
    border-collapse: collapse
    table-layout: fixed
    margin-left: auto
    opacity: 0.6

  td
    text-align: center
    padding: 8px 6px
    // font-family: system-ui
    font-family: Helvetica

  thead tr
    &:first-child td
      font-size: 3em
      font-weight: 100
      font-size: 24px

    &:last-child td
      font-size: 1rem
      padding-bottom: 10px 
      font-weight: 500 
      font-size: 18px

  tbody td
     font-weight: 400 
     font-size: 16px

  .today
    font-weight: bold
    background: rgba(#fff, 0.2)
    border-radius: 12px

  .grey
    color: #ccc
    opacity: 0.5
"""

render: -> """
  <div id="clock"></div>
  <table>
    <thead>
    </thead>
    <tbody>
    </tbody>
  </table>
"""


updateHeader: (rows, table) ->
  thead = table.find("thead")
  thead.empty()

  thead.append "<tr><td colspan='7'>#{rows[0]}</td></tr>"
  tableRow = $("<tr></tr>").appendTo(thead)
  daysOfWeek = rows[1].split(/\s+/)

  for dayOfWeek in daysOfWeek
    tableRow.append "<td>#{dayOfWeek}</td>"

updateBody: (rows, table) ->
  #Set to 1 to enable previous and next month dates, 0 to disable
  PrevAndNext = 1

  tbody = table.find("tbody")
  tbody.empty()

  rows.splice 0, 2
  rows.pop()

  today = rows.pop().split(/\s+/)
  month = today[0]
  date = today[1]
  year = today[2]

  lengths = [31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30]
  if year%4 == 0 || ( year%100 == 0 && year % 400 == 0)
    lengths[2] = 29

  for week, i in rows
    days = week.split(/\s+/).filter((day) -> day.length > 0)
    tableRow = $("<tr></tr>").appendTo(tbody)

    if i == 0 and days.length < 7
      for j in [days.length...7]
        if @otherMonths == true
          k = 6 - j
          cell = $("<td>#{lengths[month-1]-k}</td>").appendTo(tableRow)
          cell.addClass("grey")
        else
          cell = $("<td></td>").appendTo(tableRow)

    for day in days
      day = day.replace(/\D/g, '')
      cell = $("<td>#{day}</td>").appendTo(tableRow)
      cell.addClass("today") if day == date

    if i != 0 and 0 < days.length < 7 and @otherMonths == true
      for j in [1..7-days.length]
        cell = $("<td>#{j}</td>").appendTo(tableRow)
        cell.addClass("grey")

update: (output, domEl) ->
  rows = output.split("\n")
  table = $(domEl).find("table")

  @updateHeader rows, table
  @updateBody rows, table
