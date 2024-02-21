
# Appearance
width        = '367px'
barHeight    = '16px'
labelColor   = '#fff'
usedColor    = '#d7051d'
freeColor    = '#525252'
bgColor      = '#fff'
borderRadius = '1px'
bgOpacity    = 0.9

command: "last=$(curl --silent https://xkcd.com/info.0.json |
                 sed -e 's/[{}]/''/g' |
                 awk -v k=\"text\" '{ n=split($0,a,\",\");
                                      for (i=1; i<=n; i++) print a[i]}' |
                 grep '\"num\":' |
                 sed 's/:/ /1' |
                 awk -F \" \" '{ print $2 }') &&
                 newid=$((RANDOM%$last+1)) &&
                 curl --silent https://xkcd.com/$newid/info.0.json"

# Set the refresh frequency (milliseconds) to every 10 min
refreshFrequency: 600000

style: """

  // Align contents left or right
  widget-align = left
  
  // Position where you want
  bottom: 0%
  left: 0%
  width: 25%
  
  // Statics text settings
  color: #fff
  font-family Helvetica Neue
  background rgba(#000, .5)
  padding 10px 10px 15px
  border-radius 5px
  
  // Style
  .container
    text-align: widget-align
    position: relative
    clear: both

  .container:not(:first-child)
    margin-top: 20px

  .header-grid
    display: grid
    grid-template-columns: 80% 20%

  .grid-item

  .widget-title
    text-align: widget-align
    font-size: 10px
    text-transform: uppercase
    font-weight: bold

  .para
    font-size: 10px

  .date-num
    text-align: right
    font-size:  11px
    font-weight: bold

  img
    width: 100%
"""

# Render the output.
render: (output) -> """
  <div id='container'>
  <div>
"""

update: (output, domEl) -> 

  container = $(domEl).find('#container')
  xkcd = JSON.parse(output)
  date = "#{xkcd.year}-#{xkcd.month}-#{xkcd.day}-"
         .replace(/-(\d)-/, "-0$1-")
         .replace(/-(\d)-/, "-0$1-")
         .replace(/-$/, "")
  content = 
    """
    <div class="header-grid">
      <div class="grid-item">
	    <div class="widget-title">#{xkcd.title}</div>
      </div>
      <div class="date-num">
        #{xkcd.num}<br>
	    #{date}
      </div>
      <div class="para">
	    #{xkcd.alt}
      </div>
    </div>
    <img src="#{xkcd.img}"/>
    """
  $(container).html content
