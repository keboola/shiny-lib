$(function(){
    console.log("Keboola JS components");
});

// Handle messages for setting gauge
Shiny.addCustomMessageHandler("updateProgress",
                              function(message) {
                                  console.log(JSON.stringify(message));
                                  var item = "<div id='" + message.id + "_text' class='col-sm-9'>"
                                                + message.text 
                                                + "</div><div id='" + message.id + "_value' class='col-sm-3 " 
                                                + message.valueClass + "'>" 
                                                + message.value + "</div>";
                                  
                                  if ('parentId' in message) {
                                      console.log("adding container to parent: " + message.parentId)
                                      var containerId = message.parentId + "_container";
                                      if (!$("#" + containerId).length) {
                                        console.log("attaching container " + containerId);
                                        $("#" + message.parentId).append("<div id='" + containerId + "' class='container-fluid'/>");
                                      }
                                      $container = $("#" + containerId);
                                  } else {
                                      $container = $("#progress_panel");
                                  }
                                  
                                  
                                  if($("#"  + message.id).length) { 
                                      $("#" + message.id + "_text").html(message.text);
                                      $("#" + message.id + "_value").html(message.value);
                                      
                                      $("#" + message.id + "_value").addClass(message.valueClass);
                                      //$statusItem = $("#" + message.id);
                                      //$statusItem.html(item);
                                  } else {
                                      $statusItem = $("<div id='" + message.id + "' class='kbc-status-item'/>");
                                      $statusItem.html(item);
                                      $container.append($statusItem);
                                  }
                              });

