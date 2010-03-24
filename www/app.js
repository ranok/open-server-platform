$(function() {
    $.getJSON("app/osp_web/clusterwide?operation=apps",
        function(apps) {
            var app_list = "<ul>";
            $.each(apps,
                   function(index, app_name) {
                       app_list += '<li class="draggable border" id="applist-' + app_name + '">' + app_name  + '</li>';
                   });
            app_list += "</ul>";
            $("#app-list-loading").remove();
            $("#apps").append(app_list);

            $(".draggable").draggable({
       	        helper: 'clone',
                connectToSortable: ".sortable"
            });
    });

    $.getJSON("app/osp_web/clusterwide?operation=appnode",
	      function(nodes) {
		$.each(nodes,
		       function(node_num, n) {
			 var node_block = "";
			 node_block = '<div id="node-' + node_num + '" class="border node" style="float: left; margin-right: 20px;">' +
				      '  <h4 class="center">' + n.node + '</h4>' +
				      '  <ul class="sortable droppable">';
			 $.each(n.running_apps,
				function(app_num, app) {
				  node_block += '<li class="draggable border" id="app-' + app.name +'">' + app.name +
                                                '<br />Port: ' + app.port +
				                '<br /><br /><a href="#" id="stop-app-' + app.name + '" class="stop-app">Stop</a>' +
				                '</li>';
				});
			 node_block += '    <li></li>' +
				       '  </ul>' +
				       '</div>';

			 $("#node-loading").remove();
			 $("#nodes").append(node_block);
		});

		$("#nodes").append('<br style="clear: both;" />');

		$(".droppable").sortable({
		    connectWith: '.droppable',
		    stop: function(event, ui) {
		            if ($(ui.item).children("a").size() == 0) {
                                var app_name = $(ui.item).attr("id").substring(8);

                                $("#dialog").html('<form id="app-port-form" action="#"><input type="hidden" name="app-port-name" value="' + app_name +
                                                  '" /><p>Port: <input type="text" style="width: 3em;" name="app-port" id="app-port" /></p>' +
                                                  '<p><input type="submit" value="Create" /></p></form>').show().dialog();

                                $("#app-port-form").submit(function(event) {
                                    event.preventDefault();
                                    var port = $("#app-port").val();
                                    var node_name = $(ui.item).parents("ul").siblings("h4").text();
                                    var start_url = "app/osp_web/clusterwide?operation=start_app&app=" + app_name + "&port=" + port + "&node=" + node_name;

                                    $.ajax({
                                            url: start_url,
                                            success: function(msg) {
                                                if (msg == "Application started successfully") {
                                                    $("#dialog").dialog("destroy");
                                                    $(ui.item).attr("id", "app-" + app_name)
                                                    $(ui.item).append('<br />Port: ' + port + '<br /><br /><a href="#" class="stop-app">Stop</a>');
                                                }
                                                else if (msg == "There was an error starting the application") {
                                                    alert("Error starting the application.");
                                                    $("#dialog").dialog("destroy");
                                                    $(ui.item).remove();
                                                }
                                                    
                                            },
                                            error: function(msg) {
                                                alert("Error with the Start Application request.  Try again.");
                                                $("#dialog").dialog("destroy");
                                                $(ui.item).remove();
                                            }
                                    });
                                });
		            }
		          }
		});
    });

    $("body").click(function(event) {
	var $target = $(event.target);
        if ($target.is(".stop-app")) {
            event.preventDefault();
            var app_name = $target.parent().attr("id").substring(4);
	    var node_name = $target.parents("ul").siblings("h4").text();
	    var stop_url = "app/osp_web/clusterwide?operation=stop_app&app=" + app_name + "&node=" + node_name;

            $.ajax({
		    url: stop_url,
                    success: function(msg) {
                        if (msg == "Application stopped successfully")
                          $target.parent().remove();
                        else if (msg == "There was an error stopping the application")
                          alert("Error stopping the application.");
                    },
                    error: function(msg) {
                        alert("Error with the Stop Application request.  Try again.");
                    }
            });
	}
    });
});
