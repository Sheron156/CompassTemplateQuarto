#import datetime

#let entry(title, body, details) = [
    #heading(level: 2, title)
    #block(inset: (right: 2em), body)

    #text(fill: gray, details)
]

#let CompassTypst(

  // Here is where we define our variables

  // The document title.
  title: "CompassTypst",
  
  // The document subtitle.
  subtitle: "I'm a subtitle",
  
  // The document date.
  date: datetime.today(),

  // Logo in top right corner.
  typst-logo: "compass-logo.png",
  
  // Abstract.
  abstract: "Abstract goes here",
  
  // Author.
  author: "Author goes here",
  
  // Color of the footer.
  footer_color: "#D1EDEF",
  
  keywords: (),
  
  // Logo scale (in %).
  logo_scale: "100",
  
  // Columns.
  num_columns: "2",
  
  // Logo column size (in in).
  typst-logo-column-size: "10",
  
  // Title font size.
  title_font_size: "38",
  
  // Subtitle font size.
  subtitle_font_size: "22",

  // The document's content.
  body
) = {

  // Font size and others.
  
  set text(font: "Nunito", 14pt)
  
  logo_scale = int(logo_scale) * 2in
  title_font_size = int(title_font_size) * 1pt
  num_columns = int(num_columns)
  typst-logo-column-size = int(typst-logo-column-size) * 1pt
  
  // Format the date using Typst's display method.
  let formatted_date = if date != none { date.display("[month repr:long] [day], [year]") } else { "" }
  
  // Configure the pages.
  set page(
    header-ascent: 14pt,
    margin: (left: 2cm),
    numbering: "1",
    number-align: center,
    header: align(
      right + horizon,
      pad(x: -1.7cm, top: .5cm, image(typst-logo.path, width: 3cm))
    ),
    
    // This sets background color strip on right.
    background: place(right + top, rect(
      fill: rgb("#D1EDEF"),
      height: 100%,
      width: 1%,
    ))
  )
  
  // Configure headings.
set heading(numbering: "1.")
  
 show heading: set text(font: "Nunito")
 show heading.where(level: 1): set text(1.1em)
 show heading.where(level: 1): underline
 show heading.where(level: 1): set par(leading: 0.4em)
 show heading.where(level: 1): set block(below: 0.8em)

show heading.where(level: 2): it => [
    it
    // Create a grid with a horizontal line below the heading
    grid(
        columns: (1fr),
        gutter: 0pt,
        [
            // The heading content
            it
            v(0.5cm)  // Add some vertical space before the line if needed
            grid.hline(
                y: 0, 
                start: 0, 
                end: none, 
                stroke: 1pt + black, 
                position: bottom
            )
        ]
    )
    v(0.5cm)  // Add some vertical space after the line if needed
]
  
  // Links should be Compass Blue.
  show link: set text(rgb("#D1EDEF"))

  // Blue border column.
  grid(
    columns: (1fr),
  
    // Title & Subtitle.
    pad(bottom: 1cm, text(font: "Ubuntu", 20pt, weight: 800, upper(title))),
    pad(bottom: 1cm, text(font: "Nunito", 18pt, weight: 500, upper(subtitle))),

    // Published and Author in two columns.
    grid(
      columns: (1fr, 1fr),
      pad(bottom: 1cm, text(font: "Nunito", 12pt, weight: 500, "Published: " + upper(formatted_date))),
      pad(bottom: 1cm, text(font: "Nunito", 12pt, weight: 500, "Author: " + upper(author)))
    ),

    // The main body text.
    {
      set par(justify: true)
      body
      v(1fr)
    }
  )
}
