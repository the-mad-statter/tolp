Shiny.addCustomMessageHandler('closeWindow', function(data) { 
  eval(data.message) 
});
