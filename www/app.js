$(document).ready(
		  function() {
		  });

$(function() { 
	$(".draggable").draggable({helper: 'clone', connectToSortable: ".sortable"});
	$(".droppable").sortable({remove: function(event, ui) {ui.item.hide()}});
    });