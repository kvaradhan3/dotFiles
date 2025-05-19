bing   = "http://www.bing.com/HPImageArchive.aspx?format=js&idx=0&n=7&mkt=en-US"
reddit = "https://www.reddit.com/r/EarthPorn/.json?limit=50"

srcUrl    = reddit
userAgent = "User-AgentMozilla/5.0 Gecko/20100101 Firefox/29.0"
debug     = 1

command: "curl -s -A '#{userAgent}' '#{srcUrl}'"

refreshFrequency: '1h'

style: """
  position: absolute
  z-index: -10000
  left:    0px
  top:     0px
  width:   100%
  height:  100%
  color:   white
  .container
    position: absolute
    top:      0px
    left:     0px
    width:    100%
    height:   100%
    background-size: cover
  .shadow
    position: absolute
    bottom:   0px
    left:     0px
    width:    100%
    height:   25%
    background: linear-gradient( 0deg, black, rgba(192, 192, 192, 0) )
  .descr
    position:  absolute
    left:      10px
    bottom:    10px
    font-size: 11px
    color:     white
    font-family: Helvetica Neue
"""

render: -> """
  <div class="wallpaper" />
"""

update: (output, domEl) ->
  jsonData = JSON.parse(output)
  if debug
    console.log(jsonData)

  if srcUrl.match(/bing/i)
    console.log("2. Length of json array" + jsonData.images.length)
    index = Math.floor(Math.random() * 7)
    hres  = jsonData.images[index].hsh
    title = jsonData.images[index].title
    url   = "https://www.bing.com" + jsonData.images[index].url
    authr = jsonData.images[index].copyright
    console.log(String(index) + "(B): " + title + " - " + url)

  if srcUrl.match(/reddit/i)
    index = Math.floor(Math.random() * 50)
    hres  = jsonData.data.children[index].data.preview.images[0].source.width
    title = jsonData.data.children[index].data.title
    url   = jsonData.data.children[index].data.url
    authr = jsonData.data.children[index].data.author_fullname
    console.log(String(index) + "(R): " + title + " - " + url)

  if debug
    console.log(String(index) + ": " + title + " - " + url)

  $domEl = $(domEl)
  img    = new Image
  img.onload = ->
    $domEl.find('div').addClass 'old'
    $div = $('<div class="container" />')
    $div.css 'background-image', 'url("' + img.src + '")'
    $div.css 'display', 'none'
    $div.fadeIn 'slow', ->
      $domEl.find('div.old').remove()
      return
    $shadow = $('<div class="shadow" />')
    $div.append $shadow
    $descr  = $('<div class="descr" />')
    now     = new Date
    hours   = now.getHours()
    ampm    = if hours < 12 then 'a' else 'p'
    if hours > 12
      hours = hours - 12
    minutes = now.getMinutes()
    if minutes < 10
      minutes = '0'+minutes
    if title
      $descr.html title + " / " + authr + "  |  " + hours + ":" + minutes + ampm
    else
      $descr.html hours + ":" + minutes + ampm
    $div.append $descr
    $domEl.append $div
  img.src = url
