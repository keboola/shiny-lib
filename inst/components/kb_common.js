
// settings modal exit
$(document).on('hidden.bs.modal', '#kb_configModal', function (e) {
   
   $("#kb-settingsMode").val("0");
   alert("SETTINGS MODE VAL:" + $("#kb-settingsMode").val());
    console.log("end settings mode");
})
  
$(document).on('show.bs.modal', '#kb_configModal', function (e) {
   console.log(e);
   $("#kb-settingsMode").val("1"); 
   alert("SETTINGS MODE VAL:" + $("#kb-settingsMode").val());
    console.log("start settings mode");
})    

// Handle messages for loading screen
Shiny.addCustomMessageHandler(
    "updateProgress",
    function(message) {
        var item = "<div id='" + message.id + "_text' class='col-sm-9'>"
                    + message.text 
                    + "</div><div id='" + message.id + "_value' class='col-sm-3 " 
                    + message.valueClass + "'>" 
                    + message.value + "</div>";
        
        if ('parentId' in message) {
            var containerId = message.parentId + "_container";
            if (!$("#" + containerId).length) {
                $("#" + message.parentId).append("<div id='" + containerId + "' class='container-fluid'/>");
            }
            $container = $("#" + containerId);
        } else {
            $container = $("#kb_progress_panel");
        }
        if($("#"  + message.id).length) { 
            $("#" + message.id + "_text").html(message.text);
            $("#" + message.id + "_value").html(message.value);
            $("#" + message.id + "_value").addClass(message.valueClass);
        } else {
            $statusItem = $("<div id='" + message.id + "' class='kb-status-item'/>");
            $statusItem.html(item);
            $container.append($statusItem);
        }
    }
);

Shiny.addCustomMessageHandler(
    "renameButton",
    function(message) {
        $("#" + message.buttonId).html(message.text)
    }
);

