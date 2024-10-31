#show: CompassTypst.with(
$if(title)$
  title: "$title$",
$endif$
$if(subtitle)$
  subtitle: "$subtitle$",
$endif$
$if(date)$
  date: "$date$",
$endif$
$if(author)$
  author: "$author$",
$endif$
$if(abstract)$
  abstract: "$abstract$",
$endif$
$if(typst-logo)$
  typst-logo: (
    path: "$typst-logo.path$",
    caption: [$typst-logo.caption$]
  ), 
$endif$
)

