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
                                                '<br />Port: <span class="port">' + app.port + '</span>' +
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
                                                  '<p><input type="submit" value="Create" /></p></form>');
                                
                                $("#dialog").show().dialog({
                                        close: function(msg) {
                                            $(ui.item).remove();
                                            $("#dialog").dialog("destroy");
                                        }
                                });

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
                                                    $(ui.item).attr("id", "app-" + app_name);
                                                    $(ui.item).append('<br />Port: <span class="port">' + port + '</span><br /><br /><a href="#" class="stop-app">Stop</a>');
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
                    },
                    receive: function(event, ui) {
                            if ($(ui.sender).parents("div").siblings("h3").text() == "Running Nodes") {
                                var app_name =  $(ui.item).attr("id").substring(4);
                                var port = $(ui.item).children(".port").text();
                                var sender_node_name = $(ui.sender).siblings("h4").text();
                                var node_name = $(ui.item).parents("ul").siblings("h4").text();
                                var stop_url = "app/osp_web/clusterwide?operation=stop_app&app=" + app_name + "&node=" + sender_node_name;
                                var start_url = "app/osp_web/clusterwide?operation=start_app&app=" + app_name + "&port=" + port + "&node=" + node_name;

                                $.ajax({
                                        url: stop_url,
                                        success: function(msg) {
                                            if (msg == "Application stopped successfully") {
                                                $.ajax({
                                                        url: start_url,
                                                        success: function(msg) {
                                                            if (msg == "Application started successfully")
                                                                ;
                                                            else if (msg == "There was an error starting the application") {
                                                                alert("Error starting the application during migration.  The application will be reverted to the originating node.");

                                                                var revert_url = "app/osp_web/clusterwide?operation=start_app&app=" + app_name + "&port=" + port + "&node=" + sender_node_name;

                                                                $.ajax({
                                                                        url: revert_url,
                                                                        success: function(msg) {
                                                                            if (msg == "Application started successfully")
                                                                                $(ui.sender).sortable("cancel");
                                                                            else if (msg == "There was an error starting the application") {
                                                                                alert("Error reverting the application after a failed migration.  Restart the application.");
                                                                                $(ui.item).remove();
                                                                            }
                                                                        },
                                                                        error: function(msg) {
                                                                            alert("Error with the Start Application request after a failed migration attempt.");
                                                                        }
                                                                    });
                                                            }
                                                        },
                                                        error: function(msg) {
                                                            alert("Error with the Start Application request for migration.  Try again.");
                                                        }
                                                });   
                                            }
                                            else if (msg == "There was an error stopping the application")
                                                alert("Error stopping the application during migration.");
                                        },
                                        error: function(msg) {
                                            alert("Error with the Stop Application request for migration.  Try again.");
                                        }
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
