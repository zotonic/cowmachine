
	window.onload = function()
	{
		if (!!window.EventSource) {
			const date = new Date();
			document.getElementById('currentDate').innerHTML
				= date.toDateString();
			setupEventSource();
		} else {
			document.getElementById('status').innerHTML =
				"Sorry but your browser doesn't support the EventSource API";
		}
	};

function setupEventSource() {
	let evtSource;
	const btConnect = document.getElementById('connect');
	const btDisconnect = document.getElementById('disconnect');
	btDisconnect.disabled = true;
	
	btConnect.onclick = function() {
		
		btConnect.disabled = true;
		btDisconnect.disabled = false;

		evtSource = new EventSource('/eventsource');
		console.log('Start connection');
		console.log('withCredentials = ' + evtSource.withCredentials);
		console.log('readyState = ' + evtSource.readyState);
		console.log('URL = ' + evtSource.url);
		evtSource.addEventListener('message', function(event) {
			addStatus(event.data+ " ");
			}, false);
			
		
		evtSource.addEventListener('open', function(event) {
			console.log("Connection to server opened.");
			const date = new Date();
			addStatus('<b>' + date.toLocaleTimeString() +' </b><br />' +
			'<div style="color:green">EventSource server is connected.</div>' +
			'<div style="marging:2rem">server sent the following:</div>')
		}, false);
		
		evtSource.addEventListener('error', function(event) {
			console.log("Top EventSource failed. " + event);
			console.log(event);
		
			if (event.eventPhase == EventSource.CLOSED) {
				addStatus('<br/>' + 'EventSource connection was closed.<br/>')
				evtSource.removeEventListener('error', this);
			}else {
				evtSource.close();
			}
			btConnect.disabled = false;
			btDisconnect.disabled = true;	
			// setupEventSource();
		}, false);

		window.addEventListener('beforeunload', function() {
			evtSource.close();
		});
	  };

	  btDisconnect.onclick = function() {
		console.log('Connection closed');
		evtSource.close();
		addStatus('<br/>' + 'EventSource connection was closed.' + '<br/>');
		btConnect.disabled = false;
		btDisconnect.disabled = true;
	};
}

function addStatus(text) {
	document.getElementById('status').innerHTML
	= document.getElementById('status').innerHTML
	+ text + " ";
}