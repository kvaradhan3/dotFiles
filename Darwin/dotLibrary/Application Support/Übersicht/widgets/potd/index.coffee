# =============================================================================
# Übersicht wallpaper widget
#
# Sections, top to bottom:
#   1. Settings   everything general you might change
#   2. Helpers    small generic tools: shell quoting, curl, text cleanup
#   3. Sources    one self-contained block per picture source, each holding
#                 its own settings, how to fetch it, and how to read the reply
#   4. Display    how a picked picture is drawn and captioned
#   5. Command    turns the chosen sources into one shell command
#   6. Widget     the part Übersicht calls
#
# Übersicht rule: every "name = value" line must come BEFORE the first
# "key: value" line in section 6. Anything assigned after it splits the
# widget in two and Übersicht silently drops 'command'.
# =============================================================================


# =============================================================================
# 1. Settings
# =============================================================================
settings =
  source:        'random'           # a source name from section 3, or 'random'
  rotation:      ['commons', 'nasa', 'unsplash', 'bing']   # used by 'random'
  refresh:       60 * 60 * 1000
  keyDir:        '$HOME/.config/wallpaper'      # API key files live here
  userAgent:     'UbersichtWallpaper/1.0 (kannan@ieee.org)'
  curlOptions:   ['--silent', '--fail', '--location', '--max-time', '60']
  landscapeOnly: true               # skip pictures taller than they are wide
  lastFile:      '/tmp/wallpaper-last.json'     # copy of the last fetch
  logFile:       '/tmp/wallpaper.log'           # what update() decided
  fadeSpeed:     'slow'
  caption:
    titleMax:    140
    creditMax:   60
    ellipsis:    '…'
    joiner:      ' / '
    clockJoiner: '  |  '


# =============================================================================
# 2. Helpers
# =============================================================================

# --- Shell building ----------------------------------------------------------
# Values are single-quoted, so the shell takes them literally. Values made
# with atRunTime() are shell code evaluated each time the command runs
# (random numbers, dates, reading key files), so they are double-quoted.

shq = (s) -> "'" + String(s).replace(/'/g, "'\\''") + "'"

atRunTime = (code) -> { shell: code }

shArg = (v) -> if v?.shell? then "\"#{v.shell}\"" else shq(v)

shJoin = (prefix, v) ->
  if v?.shell? then atRunTime(prefix + v.shell) else prefix + v

randomInt = (n, offset = 0) -> "$((RANDOM % #{n} + #{offset}))"

# Shell code printing one list item, chosen at random on each run.
# The "(0)" pattern form keeps macOS's bash 3.2 happy inside $( ).
randomPick = (items) ->
  cases = ("(#{i}) printf '%s' #{shq(x)} ;;" for x, i in items).join(' ')
  "$(case #{randomInt(items.length)} in #{cases} esac)"

readKey = (file) -> "$(cat \"#{settings.keyDir}/#{file}\" 2>/dev/null)"

# curl --get <url> with headers and URL-encoded parameters
curl = ({ url, params = {}, headers = {}, output }) ->
  parts = (shq(o) for o in settings.curlOptions)
  parts.push '--user-agent', shq(settings.userAgent)
  parts.push '--output', shq(output) if output
  parts.push '--header', shArg(shJoin("#{k}: ", v)) for k, v of headers
  parts.push '--data-urlencode', shArg(shJoin("#{k}=", v)) for k, v of params
  parts.push '--get', shArg(url)
  'curl ' + parts.join(' ')

# --- Text and picture checks -------------------------------------------------
pickRandom = (list) -> list[Math.floor(Math.random() * list.length)]

pathOnly = (url) -> String(url).split('?')[0]     # drop "?utm_..." tracking

toQuery = (obj) ->
  ("#{encodeURIComponent(k)}=#{encodeURIComponent(v)}" for k, v of obj).join('&')

isLandscape = (w, h) -> not settings.landscapeOnly or not (w and h) or w >= h

screenWidth = -> Math.round(window.screen.width * (window.devicePixelRatio or 1))

# HTML (entities, tags, extra whitespace) -> plain text
unhtml = (s) ->
  return '' unless s
  t = document.createElement('textarea')
  t.innerHTML = s
  t.value.replace(/<[^>]*>/g, '').replace(/\s+/g, ' ').trim()

shorten = (s, n) ->
  s = unhtml(s)
  return s if s.length <= n
  s[0...n].replace(/\s+\S*$/, '') + settings.caption.ellipsis


# =============================================================================
# 3. Sources
#
# Each source is one object with:
#   settings   plain values (URLs, sizes, wording) used only by this source
#   fetch()    returns the shell command that prints the source's JSON
#   parse(d)   turns that JSON into { url, title, credit } or null
#   afterShow  (optional) returns a shell command to run once shown
# =============================================================================

# --- Wikimedia Commons: random featured pictures from chosen categories -----
# No key. Wikimedia asks for a real contact address in settings.userAgent.
commons =
  api:        'https://commons.wikimedia.org/w/api.php'
  # One search is picked at random each refresh:
  #   incategory:"Name"     files directly in that category (tight)
  #   incategory:"A|B|C"    files directly in any of A, B or C
  #   deepcat:"Name"        also all subcategories, any depth (loose)
  # coffeelint: disable=max_line_length
  searches: [
    'incategory:"Featured pictures of landscapes|Featured pictures of mountains|Featured pictures of coasts|Featured pictures of volcanoes|Featured pictures of islands"'
    'deepcat:"Featured pictures of history"'
  ]
  # coffeelint: enable=max_line_length
  searchSuffix:  'filetype:bitmap'
  fileNamespace: 6                  # MediaWiki namespace number for "File:"
  batch:         10                 # random candidates per request
  width:         3840               # ask for a copy this wide, not the original
  mimes:         ['image/jpeg', 'image/png']
  metaFields:    ['ImageDescription', 'ObjectName', 'Artist']
  defaultCredit: 'Wikimedia Commons'

  fetch: ->
    curl
      url: @api
      params:
        action:              'query'
        format:              'json'
        formatversion:       2
        generator:           'search'
        gsrnamespace:        @fileNamespace
        gsrsort:             'random'
        gsrlimit:            @batch
        gsrsearch:           atRunTime(randomPick("#{s} #{@searchSuffix}" for s in @searches))
        prop:                'imageinfo'
        iiprop:              'url|size|mime|extmetadata'
        iiextmetadatafilter: @metaFields.join('|')
        iiurlwidth:          @width

  parse: (d) ->
    pages = d?.query?.pages
    return null unless Array.isArray(pages)
    usable = []
    for pg in pages
      ii = pg.imageinfo?[0]
      continue unless ii and ii.mime in @mimes and isLandscape(ii.width, ii.height)
      usable.push { pg, ii }
    return null unless usable.length
    { pg, ii } = pickRandom(usable)
    meta = ii.extmetadata or {}
    name = pg.title.replace(/^File:/, '').replace(/\.[^.]+$/, '')
    url:      pathOnly(ii.thumburl or ii.url)
    fallback: pathOnly(ii.url)        # full original, if the sized copy fails
    title:    shorten(meta.ImageDescription?.value or meta.ObjectName?.value or name,
                      settings.caption.titleMax)
    credit:   shorten(meta.Artist?.value or @defaultCredit, settings.caption.creditMax)


# --- Wikipedia Picture of the Day: any subject, one per day ------------------
# No key. Uses macOS 'date -v' to pick a random day.
potd =
  feed:          'https://en.wikipedia.org/api/rest_v1/feed/featured'
  daysBack:      365
  dateFormat:    '+%Y/%m/%d'
  extensions:    /\.(jpe?g|png)$/i
  defaultCredit: 'Wikimedia Commons'

  fetch: ->
    curl url: atRunTime("#{@feed}/$(date -v-#{randomInt(@daysBack)}d #{@dateFormat})")

  parse: (d) ->
    img = d?.image
    src = img?.image?.source
    return null unless src and @extensions.test(pathOnly(src))
    return null unless isLandscape(img.image.width, img.image.height)
    url:    pathOnly(src)
    title:  shorten(img.description?.text or img.title, settings.caption.titleMax)
    credit: shorten(img.artist?.text or @defaultCredit, settings.caption.creditMax)


# --- NASA Astronomy Picture of the Day -------------------------------------
# No key. Moved to science.nasa.gov in Sep 2026 (old api.nasa.gov endpoint
# now returns a NASA-logo placeholder).
nasa =
  api:           'https://science.nasa.gov/wp-json/wp/v2/apod-basic'
  perPage:       20
  pages:         100                # random page out of this many (≈ 5 years)
  fields:        ['date', 'title', 'media_type', 'hdurl', 'credit', 'copyright']
  mediaType:     'image'
  defaultCredit: 'NASA APOD'

  fetch: ->
    curl
      url: @api
      params:
        per_page: @perPage
        page:     atRunTime(randomInt(@pages, 1))
        _fields:  @fields.join(',')

  parse: (list) ->
    return null unless Array.isArray(list)
    imgs = (x for x in list when x.media_type is @mediaType and x.hdurl)
    return null unless imgs.length
    x = pickRandom(imgs)
    url:    unhtml(x.hdurl)          # "url" is the article page, not the image
    title:  shorten(x.title, settings.caption.titleMax)
    credit: shorten(x.copyright or x.credit or @defaultCredit, settings.caption.creditMax)


# --- Unsplash ---------------------------------------------------------------
# Needs an access key in <keyDir>/unsplash.key. Their rules: hotlink their
# image URLs, credit the photographer, and ping download_location on use.
unsplash =
  api:        'https://api.unsplash.com/photos/random'
  keyFile:    'unsplash.key'
  apiVersion: 'v1'
  search:
    orientation:    'landscape'
    content_filter: 'high'
    query:          'landscape'
  image:                            # resizing options added to the photo URL
    fm: 'jpg'
    q:  85
  creditFormat: (name) -> "Photo by #{name} on Unsplash"

  headers: ->
    'Authorization':  atRunTime("Client-ID #{readKey(@keyFile)}")
    'Accept-Version': @apiVersion

  fetch: -> curl url: @api, params: @search, headers: @headers()

  parse: (p) ->
    return null unless p?.urls?.raw
    sizing = Object.assign({ w: screenWidth() }, @image)
    url:    "#{p.urls.raw}&#{toQuery(sizing)}"
    title:  shorten(p.description or p.alt_description, settings.caption.titleMax)
    credit: @creditFormat(p.user?.name)
    ping:   p.links?.download_location

  afterShow: (pick) ->
    curl(url: pick.ping, headers: @headers(), output: '/dev/null') if pick.ping


# --- Bing home-page images --------------------------------------------------
# No key. Last 8 days only, so pictures repeat within about a week.
bing =
  api:   'https://www.bing.com/HPImageArchive.aspx'
  host:  'https://www.bing.com'     # image paths in the reply are relative
  params:
    format: 'js'
    idx:    0                       # 0 = start from today
    n:      8                       # days to fetch (Bing's maximum)
    mkt:    'en-US'                 # region: 'en-GB', 'de-DE', 'ja-JP', ...
  sizeSuffix:    '_UHD.jpg'         # 4K version of each image
  creditPattern: /^(.*?)\s*\(©\s*(.*)\)\s*$/    # "Place (© Photographer/Agency)"
  defaultCredit: 'Bing'

  fetch: -> curl url: @api, params: @params

  parse: (d) ->
    imgs = d?.images
    return null unless Array.isArray(imgs) and imgs.length
    x = pickRandom(imgs)
    return null unless x.urlbase or x.url
    m = (x.copyright or '').match(@creditPattern)
    url:    if x.urlbase then "#{@host}#{x.urlbase}#{@sizeSuffix}" else "#{@host}#{x.url}"
    title:  shorten((if m then m[1] else x.title or x.copyright), settings.caption.titleMax)
    credit: if m then m[2] else @defaultCredit


# --- Registry: the names usable in settings.source / settings.rotation ------
sources = { commons, potd, nasa, unsplash, bing }


# =============================================================================
# 4. Display
# =============================================================================
clockText = (now = new Date) ->
  h = now.getHours()
  ampm = if h < 12 then 'a' else 'p'
  h = h % 12 or 12
  m = String(now.getMinutes()).padStart(2, '0')
  "#{h}:#{m}#{ampm}"

captionText = (pick) ->
  c = settings.caption
  parts = (p for p in [pick.source, pick.title, pick.credit] when p)
  (if parts.length then parts.join(c.joiner) + c.clockJoiner else '') + clockText()

# Load the picture off-screen; once loaded, fade it in over the old one.
showPicture = (domEl, pick, log, onShown) ->
  $domEl  = $(domEl)
  img     = new Image
  started = Date.now()

  img.onerror = ->
    log "image FAILED to load: #{img.src}"
    if pick.fallback and img.src isnt pick.fallback
      log "trying original: #{pick.fallback}"
      img.src = pick.fallback

  img.onload = ->
    secs = Math.round((Date.now() - started) / 1000)
    log "shown (#{img.naturalWidth}x#{img.naturalHeight}, #{secs}s): #{pick.title}"
    $domEl.find('div').addClass 'old'
    $div = $('<div class="container" />')
    $div.css 'background-image', "url(\"#{img.src}\")"
    $div.css 'display', 'none'
    $div.append $('<div class="shadow" />')
    $div.append $('<div class="descr" />').text(captionText(pick))
    $domEl.append $div
    $div.fadeIn settings.fadeSpeed, -> $domEl.find('div.old').remove()
    onShown?()

  img.src = pick.url


# =============================================================================
# 5. Command: pick a source in the shell, fetch it, and print
#    {"src": "<name>", "data": <reply or null>}
# =============================================================================
buildCommand = ->
  wanted   = if settings.source is 'random' then settings.rotation else [settings.source]
  names    = (n for n in wanted when sources[n]?)
  branches = ("(#{i}) SRC=#{shq(n)}; BODY=$(#{sources[n].fetch()}) ;;" for n, i in names)
  """
  SRC=none
  BODY=
  case #{randomInt(Math.max(names.length, 1))} in
    #{branches.join('\n  ')}
  esac
  printf '{"src":"%s","data":%s}' "$SRC" "${BODY:-null}" | tee #{shq(settings.lastFile)}
  """

logLine = (run, msg) ->
  console.log 'wallpaper: ' + msg
  run "printf '%s %s\\n' \"$(date +%FT%T)\" #{shq(msg)} >> #{shq(settings.logFile)}"


# =============================================================================
# 6. Widget (Übersicht calls these; keep all "name = value" lines above)
# =============================================================================
command: buildCommand()

refreshFrequency: settings.refresh

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
    background-position: center
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
    max-width: 70%
    font-size: 11px
    color:     white
    font-family: Helvetica Neue
    white-space: nowrap
    overflow:  hidden
    text-overflow: ellipsis
"""

render: -> ""

update: (output, domEl) ->
  run = (cmd) => @run cmd
  log = (msg) -> logLine(run, msg)

  try
    res = JSON.parse(output)
  catch
    return log "unparsable output: #{output?[0...200]}"

  source = sources[res.src]
  unless source
    return log "no source named '#{res.src}' - check settings.source and settings.rotation"
  if res.data is null
    return log "#{res.src}: fetch failed (data is null) - keeping current wallpaper"

  pick = source.parse(res.data)
  unless pick?.url
    return log "#{res.src}: no usable image in response - keeping current wallpaper"
  pick.source = source.label or res.src

  srcLog = (msg) -> log "#{res.src}: #{msg}"
  srcLog "loading #{pick.url}"
  showPicture domEl, pick, srcLog, ->
    after = source.afterShow?(pick)
    run after if after
