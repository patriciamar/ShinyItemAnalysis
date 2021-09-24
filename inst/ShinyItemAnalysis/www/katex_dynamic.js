// rerender whole document on any shiny output update
$(document).on('shiny:value', (event) => {
  setTimeout(() => {renderMathInElement(event.target, {
    ignoredClasses: ["plotly"]
  });}, 0);
});
