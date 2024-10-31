// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): block.with(
    fill: luma(230), 
    width: 100%, 
    inset: 8pt, 
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    new_title_block +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: white, width: 100%, inset: 8pt, body))
      }
    )
}


#let CompassTypst(

  // Here is where we define our variables
  
  // The document title.
  
  title: "The Report's Title",
  
  // The document subtitle
  
  subtitle: none,
  
  // The document date
  date: datetime.today(),

  // Logo in top right corner.
  typst-logo: none,
  
  // Abstract.
  abstract: none,
  
  // Author
  author: none,
  
  // Color of the footer
  footer_color: "#8CD3D6",
  
  keywords: (),
  
  // Title font size
  
  title_font_size: "38",
  
  // Subtitle font size
  
  subtitle_font_size: "22",


  // The documents content.
  
  
  body
  
  ) = {

  // Font size and others
  
  set text(font: "Nunito", 11pt)
  
  title_font_size = int(title_font_size) * 1pt


  // Configure the pages.
  
  set page(
 
  // margin: (left: 25%),
   
    numbering: "1",
    number-align: center,
    
    header: align(
    right + horizon,
    pad(x: -1.7cm, top: .5cm, image(typst-logo.path, width: 3.9cm))
    ),
    
  // This sets background color strip on right
  
  background: place(right + top, rect(
      fill: rgb("#CEECED"),
      height: 100%,
      width: 1%,
    ))
  )
  
// Configure headings.
  
// Configure headings.
  set heading(numbering: "1.")
  
  show heading: set text(font: "Nunito", 22pt, weight: 800)
  show heading.where(level: 1): set text(1.1em)
  // show heading.where(level: 1): underline
  show heading.where(level: 1): set par(leading: 0.4em)
  show heading.where(level: 1): set block(below: 0.8em)
  

  // Links should be Compass Blue.
  
  show link: set text(rgb("#D1EDEF"))


  // blue border column
  
  grid(
    columns: (1fr),
  
   
    // Title & Subtitle.
    
   pad(bottom: .5cm, text(font: "Ubuntu", 24pt, weight: 800, upper(title))),
   pad(bottom: .5cm, text(font: "Nunito", 14pt, weight: 500, upper(subtitle))),
   
   // Published and Author in two columns.
   
   
    grid(
      columns: (1fr, 1fr),
      pad(bottom: 1cm, text(font: "Nunito", 12pt, weight: 500, "Published: " + date)),
      pad(bottom: 1cm, text(font: "Nunito", 12pt, weight: 500, "Author: " + author)),
    ),

    // The main body text.
    {
      set par(justify: true)
      body
      v(1fr)
    },
  

  )
}

#show: CompassTypst.with(
  title: "Routine Reports",
  subtitle: "Weekly, Monthly & Quarterly",
  date: "2024-10-29",
  author: "Office of System Performance",
  typst-logo: (
    path: "compass-logo.png",
    caption: []
  ), 
)

#import "@preview/fontawesome:0.1.0": *


#block[
#callout(
body: 
[
The following tables contain the #emph[weekly];, #emph[monthly] and #emph[quarterly] reports that must be routinely generated. The name of the report, the frequency as well as purpose, source data location and data destinations are all listed. Links to tutorials on how to generate each report are also included.

For detailed tutorials on how to complete each report, please see the manual on the #link("M:\\DATA%20&%20REPORTS\\Compass%20Data%20Manual")[#strong[master drive];];.

]
, 
title: 
[
#strong[Description]
]
, 
background_color: 
rgb("#ffe5d0")
, 
icon_color: 
rgb("#FC5300")
, 
icon: 
fa-fire()
)
]

= #strong[Weekly Reports]
<weekly-reports>
#[
#let nhead = 1;
#let nrow = 10;
#let ncol = 6;

  #let fill-array = ( 
    // tinytable cell fill after
    (y: 0, x: 5, fill: rgb("#4AB8C1")),
    (y: 0, x: 4, fill: rgb("#4AB8C1")),
    (y: 0, x: 3, fill: rgb("#4AB8C1")),
    (y: 0, x: 2, fill: rgb("#4AB8C1")),
    (y: 0, x: 1, fill: rgb("#4AB8C1")),
    (y: 0, x: 0, fill: rgb("#4AB8C1")),
  )
  #let style-array = ( 
    // tinytable cell style after
    (y: 0, x: 5, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 4, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 3, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 2, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 1, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 0, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 10, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 10, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 10, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 10, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 10, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 10, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 10, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 9, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 8, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 7, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 6, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 5, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 4, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 3, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 2, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 1, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 0, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
  )
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x == it.x and data.y == it.y)
    if data != none {
      set text(data.color)
      set text(data.fontsize)
      if data.underline == true { tmp = underline(tmp) }
      if data.italic == true { tmp = emph(tmp) }
      if data.bold == true { tmp = strong(tmp) }
      if data.mono == true { tmp = math.mono(tmp) }
      if data.strikeout == true { tmp = strike(tmp) }
      tmp
    } else {
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
align: (left, center, left, left, center, center),
    columns: (auto, auto, auto, auto, auto, auto),
    stroke: none,
    fill: (x, y) => {
      let data = fill-array.find(data => data.x == x and data.y == y)
      if data != none {
        data.fill
      }
    },

    // tinytable lines after
table.hline(y: 11, start: 0, end: 6, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 6, stroke: 0.05em + black),
table.hline(y: 0, start: 0, end: 6, stroke: 0.1em + black),
table.vline(x: 0, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 6, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 5, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 4, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 3, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 2, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 1, start: 0, end: 1, stroke: 0.1em + white),
table.hline(y: 0, start: 0, end: 6, stroke: 0.1em + white),
table.hline(y: 1, start: 0, end: 6, stroke: 0.1em + white),

    table.header(
      repeat: true,
[Report], [Frequency], [Purpose], [Source Data Location], [Data Destination], [Int vs Ext],
    ),

    // tinytable cell content after
[ASEBA Score Refresh          ], [Weekly], [Pulls ASEBA calculated scores for BI dashboard stats                          ], [ASEBA website                                            ], [Master], [Internal],
[Program History Refresh      ], [Weekly], [Updates client information, status and program information to the BI dashboard], [EMHware - Detailed Program History and Client Data report], [Master], [Internal],
[Schoolboard Data Refresh     ], [Weekly], [Updates school and school board related info to the dashboard                 ], [EMHware - Educational School Status Custom report        ], [Master], [Internal],
[Closing Summary Refresh      ], [Weekly], [Pulls closing summary information to dashboard                                ], [EMHware - Case Data Data Extract in Column Format report ], [Master], [Internal],
[ChyMH Data Refresh           ], [Weekly], [Pulls interRAI chymh and screener results to dashboard                        ], [EMHware - interRAI ChYMH Reports                         ], [Master], [Internal],
[Unauthenticated ChYMHs Audit ], [Weekly], [Pulls non-authenticated chymh info to dashboard                               ], [EMHware - interRAI ChYMH Reports                         ], [Master], [Internal],
[Unauthenticated Records Audit], [Weekly], [Pull a list of all unauthenticated records by client and worker               ], [EMHware - Records Audit Report                           ], [Master], [Internal],
[Time Entry Refresh           ], [Weekly], [Pulls clinical time entry data                                                ], [EMHware - Detailed Contacts by Activity Custom Report    ], [Master], [Internal],
[BI Dash Action Log           ], [Weekly], [Refreshes pull date in BI dash                                                ], [Master                                                   ], [Master], [Internal],
[Power BI Dash Refresh        ], [Weekly], [This will refresh the BI dashboard to reflect the new weekly data             ], [Master                                                   ], [Master], [Internal],

    table.footer(
      repeat: false,
      // tinytable notes after
    ),

  ) // end table

  ]) // end align

]
= #strong[Monthly Reports]
<monthly-reports>
#[
#let nhead = 1;
#let nrow = 3;
#let ncol = 7;

  #let fill-array = ( 
    // tinytable cell fill after
    (y: 0, x: 6, fill: rgb("#4AB8C1")),
    (y: 0, x: 5, fill: rgb("#4AB8C1")),
    (y: 0, x: 4, fill: rgb("#4AB8C1")),
    (y: 0, x: 3, fill: rgb("#4AB8C1")),
    (y: 0, x: 2, fill: rgb("#4AB8C1")),
    (y: 0, x: 1, fill: rgb("#4AB8C1")),
    (y: 0, x: 0, fill: rgb("#4AB8C1")),
  )
  #let style-array = ( 
    // tinytable cell style after
    (y: 0, x: 6, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 5, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 4, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 3, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 2, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 1, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 0, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 2, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 1, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 0, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
  )
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x == it.x and data.y == it.y)
    if data != none {
      set text(data.color)
      set text(data.fontsize)
      if data.underline == true { tmp = underline(tmp) }
      if data.italic == true { tmp = emph(tmp) }
      if data.bold == true { tmp = strong(tmp) }
      if data.mono == true { tmp = math.mono(tmp) }
      if data.strikeout == true { tmp = strike(tmp) }
      tmp
    } else {
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
align: (left, center, left, left, center, center, left),
    columns: (auto, auto, auto, auto, auto, auto, auto),
    stroke: none,
    fill: (x, y) => {
      let data = fill-array.find(data => data.x == x and data.y == y)
      if data != none {
        data.fill
      }
    },

    // tinytable lines after
table.hline(y: 4, start: 0, end: 7, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 7, stroke: 0.05em + black),
table.hline(y: 0, start: 0, end: 7, stroke: 0.1em + black),
table.vline(x: 0, start: 0, end: 1, stroke: 0.01em + white),
table.vline(x: 7, start: 0, end: 1, stroke: 0.01em + white),
table.vline(x: 6, start: 0, end: 1, stroke: 0.01em + white),
table.vline(x: 5, start: 0, end: 1, stroke: 0.01em + white),
table.vline(x: 4, start: 0, end: 1, stroke: 0.01em + white),
table.vline(x: 3, start: 0, end: 1, stroke: 0.01em + white),
table.vline(x: 2, start: 0, end: 1, stroke: 0.01em + white),
table.vline(x: 1, start: 0, end: 1, stroke: 0.01em + white),
table.hline(y: 0, start: 0, end: 7, stroke: 0.01em + white),
table.hline(y: 1, start: 0, end: 7, stroke: 0.01em + white),

    table.header(
      repeat: true,
[Report], [Frequency], [Purpose], [Source Data Location], [Data Destination], [Int vs Ext], [Report to be sent to],
    ),

    // tinytable cell content after
[Case Review & Psych Consult], [Monthly], [List of complex cases that may benefit from a psychological consult], [EMHware & Master Drive                        ], [server  ], [Internal], [Alana Jackson, Nicholas Schwabe, Michelle Gauthier and all clinical managers],
[Training Feedback Dashboard], [Monthly], [Refreshes the training feedback BI dashboard                       ], [Survey Monkey                                 ], [server  ], [Internal], [Subscription sent out via powerBI dash                                      ],
[Staff Focused Audits       ], [Monthly], [Staff focused audits                                               ], [Emhware File Audit Report and  Reference Table], [computer], [Internal], [Supervisors                                                                 ],

    table.footer(
      repeat: false,
      // tinytable notes after
    ),

  ) // end table

  ]) // end align

]
= #strong[Quarterly Reports]
<quarterly-reports>
#[
#let nhead = 1;
#let nrow = 9;
#let ncol = 7;

  #let fill-array = ( 
    // tinytable cell fill after
    (y: 0, x: 6, fill: rgb("#4AB8C1")),
    (y: 0, x: 5, fill: rgb("#4AB8C1")),
    (y: 0, x: 4, fill: rgb("#4AB8C1")),
    (y: 0, x: 3, fill: rgb("#4AB8C1")),
    (y: 0, x: 2, fill: rgb("#4AB8C1")),
    (y: 0, x: 1, fill: rgb("#4AB8C1")),
    (y: 0, x: 0, fill: rgb("#4AB8C1")),
  )
  #let style-array = ( 
    // tinytable cell style after
    (y: 0, x: 6, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 5, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 4, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 3, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 2, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 1, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 0, x: 0, color: white, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 8, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 7, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 6, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 5, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 4, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 3, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 2, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 6, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 5, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 4, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 3, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 2, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 1, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 1, x: 0, color: black, underline: false, italic: false, bold: false, mono: false, strikeout: false, fontsize: 0.7em),
    (y: 9, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 8, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 7, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 6, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 5, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 4, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 3, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 2, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 1, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
    (y: 0, x: 0, color: black, underline: false, italic: false, bold: true, mono: false, strikeout: false, fontsize: 1em),
  )
  #show table.cell: it => {
    let tmp = it
    let data = style-array.find(data => data.x == it.x and data.y == it.y)
    if data != none {
      set text(data.color)
      set text(data.fontsize)
      if data.underline == true { tmp = underline(tmp) }
      if data.italic == true { tmp = emph(tmp) }
      if data.bold == true { tmp = strong(tmp) }
      if data.mono == true { tmp = math.mono(tmp) }
      if data.strikeout == true { tmp = strike(tmp) }
      tmp
    } else {
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
align: (left, center, left, left, left, center, left),
    columns: (auto, auto, auto, auto, auto, auto, auto),
    stroke: none,
    fill: (x, y) => {
      let data = fill-array.find(data => data.x == x and data.y == y)
      if data != none {
        data.fill
      }
    },

    // tinytable lines after
table.hline(y: 10, start: 0, end: 7, stroke: 0.1em + black),
table.hline(y: 1, start: 0, end: 7, stroke: 0.05em + black),
table.hline(y: 0, start: 0, end: 7, stroke: 0.1em + black),
table.vline(x: 0, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 7, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 6, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 5, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 4, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 3, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 2, start: 0, end: 1, stroke: 0.1em + white),
table.vline(x: 1, start: 0, end: 1, stroke: 0.1em + white),
table.hline(y: 0, start: 0, end: 7, stroke: 0.1em + white),
table.hline(y: 1, start: 0, end: 7, stroke: 0.1em + white),

    table.header(
      repeat: true,
[Report], [Frequency], [Purpose], [Source Data Location], [Data Destination], [Int vs Ext], [Report to be sent to],
    ),

    // tinytable cell content after
[OPOC Extract                        ], [Quarterly], [Pulls client satisfaction data from OPOC server                  ], [ BI Solution Website                          ], [Master                        ], [Internal], [NA                                                        ],
[Section 34 Quarterlies              ], [Quarterly], [Provides counts of Section 34 assessments coordinated via Compass], [Asana: Section 34                             ], [Asana: Section 34 Task        ], [Internal], [NA                                                        ],
[CCN Wait List                       ], [Quarterly], [Pulls case coordination wait list data                           ], [EMHware                                       ], [Master                        ], [Internal], [Director of Clinical & Client Services & Michelle Gauthier],
[FRIENDS Quarterly Report            ], [Quarterly], [Pulls and sends FRIENDS group data to licensing agency           ], [Lise Longchamp                                ], [Master                        ], [External], [Friends Organization                                      ],
[SAP Data for Board Report           ], [Quarterly], [Pulls  supervision data for bi-annual reporting                  ], [iSaid                                         ], [Asana: SAP Task               ], [Internal], [NA                                                        ],
[BI Quarterly Submission             ], [Quarterly], [Mandatory reporting of programs funded by the Ministry           ], [EMHware                                       ], [Lead Agency and Ministry Site ], [External], [NA                                                        ],
[BI Data Quality Reports             ], [Quarterly], [Investigates data quality before BI submission                   ], [EMHware                                       ], [Admin Assistants              ], [Internal], [Tracy Bracken                                             ],
[Targets & Interim Actuals MOH Report], [Quarterly], [Mandatory Bi-Annual reporting of KPIs                            ], [EMHware and Master Drive                      ], [Master                        ], [Internal], [Nicholas Schwabe                                          ],
[CCA Accreditation Audits            ], [Quarterly], [Accreditation audits                                             ], [Emhware File Audit Report and  Reference Table], [EMHware - supervision accounts], [Internal], [Supervisors                                               ],

    table.footer(
      repeat: false,
      // tinytable notes after
    ),

  ) // end table

  ]) // end align

]



