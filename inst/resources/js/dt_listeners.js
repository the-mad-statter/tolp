function add_length_listener(table) { 
    table.on('length.dt', function (e, settings, len) { 
        Shiny.onInputChange('page_length', len); 
    }); 
};
