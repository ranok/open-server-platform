$(document).ready(
		  function() {
		  });

$(function() { 
	$(".draggable").draggable({helper: 'clone'});
	$("#nodes").droppable({drop: function(event, ui) {
		    ui.draggable.hide("puff", {}, 500, function() {});
		}});
	$(".droppable").droppable({greedy: true, drop: function(event, ui) {
		    ui.draggable.clone(true).draggable().insertAfter(this).after("<br />");
		}});
    });