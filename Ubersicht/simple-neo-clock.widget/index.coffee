###
neomorphic simpleClock with inspiration from the simple-clock widget
Please enjoy :]
###

stylingOptions =
  # background color
  background: 'linear-gradient(145deg, #e6e6e6, #ffffff);' 
  # show fullscreen -> true
  fullscreen: false
  # display position 'top', 'middle', 'bottom'
  vertical: 'top'

dateOptions =
  # display not only 'time' also 'date'
  showDate: false
  # format of 'date'
  date: '%d/%m/%Y %a'

format = (->
  if dateOptions.showDate
    dateOptions.date + '\n' +'%l:%M %p'
  else
    '%l:%M %p'
)()

command: "date +\"#{format}\""

# the refresh frequency in milliseconds
refreshFrequency: 30000

# for update function
dateOptions: dateOptions

render: (output) -> """
  <div id='simpleClock'>#{output}</div>
"""

update: (output) ->
  if this.dateOptions.showDate
    data = output.split('\n')

    html = data[1]
    html += '<span class="date">'
    html += data[0]
    html += '</span>'
    
  else
    html = output

  $(simpleClock).html(html)

style: (->
  fontSize = '4em'
  width = 'auto'
  transform = 'auto'
  bottom = '3%'
  top = 'auto'

  if stylingOptions.fullscreen
    fontSize = '10em'
    width = '94%'

  if stylingOptions.vertical is 'middle'
    transform = 'translateY(50%)'
    bottom = '50%'
  else if stylingOptions.vertical is 'top'
    bottom = 'auto'
    top = '4.5%'

  return """
    background: #{stylingOptions.background}
    border-radius 35px
    color: #000000
    font-family: Helvetica Neue
    left: 3%
    top: #{top}
    bottom: #{bottom}
    transform: #{transform}
    width: #{width}
    box-shadow: 15px 15px 28px #b0b0b0, 
                -15px -15px 28px #ffffff;

    #simpleClock
      font-size: #{fontSize}
      font-weight: 100
      margin: 0
      text-align: center
      padding: 10px 20px

    #simpleClock .date
      margin-left: .5em
      font-size: .5em
  """
)()
