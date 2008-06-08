/*
Comcast Cable Confidential & Proprietary

CONFIDENTIALITY STATEMENT

The information contained herein is proprietary to Comcast 
and may not be used, reproduced or disclosed to others 
except as specifically permitted in writing by Comcast.  
The recipient of this document, by its retention and use, 
agrees to protect the information contained.
*/

/*
updates to handle xiff3beta1.  
(note that xiff3beta1 needed the digest auth patch still.)
There were Roster and related exposures that the API change exposed;
those need to be abstracted.
*/

package com.machenmusik.im
{
	import flash.events.Event;
	import flash.events.EventDispatcher;
	import flash.utils.ByteArray;
	
	import mx.events.CollectionEvent;
	import mx.rpc.events.FaultEvent;
	import mx.rpc.events.ResultEvent;
	import mx.rpc.http.HTTPService;

	import org.jivesoftware.xiff.core.JID;
	import org.jivesoftware.xiff.core.XMPPConnection;
	import org.jivesoftware.xiff.data.Message;
	import org.jivesoftware.xiff.events.*;
	
	import org.jivesoftware.xiff.im.Roster;
	import org.jivesoftware.xiff.data.im.RosterGroup;
	import org.jivesoftware.xiff.data.im.RosterItemVO;

	import com.machenmusik.CM_IM_Decrypt;
	import flash.utils.Timer;
	import flash.events.TimerEvent;
    
	public class Connection extends EventDispatcher
	{
		[Event(name="connect", type="Event")]
		[Event(name="login", type="Event")]
		[Event(name="disconnect", type="Event")]
		[Event(name="incoming", type="XMLEvent")]
		[Event(name="message", type="XMLEvent")]
		[Event(name="jingle", type="XMLEvent")]
		[Event(name="outgoing", type="XMLEvent")]
		[Event(name="error", type="XMLEvent")]
		[Event(name="presence", type="XMLEvent")]
		[Event(name="roster", type="XMLEvent")]
		
		public function get username(): String { return _xiffconn.username; }		
		public function set username(s: String): void { _xiffconn.username = s; }		

		public function get server(): String { return _xiffconn.server; }		
		public function set server(s: String): void { _xiffconn.server = s; }		

		public function get password(): String { return _xiffconn.password; }		
		public function set password(s: String): void { _xiffconn.password = s; }		

		public function get realm(): String { return _realm; }		
		public function set realm(s: String): void { _realm = s; }				
		
		public function get tag(): String { return _tag; }		
		public function set tag(s: String): void { _tag = s; }				
		
		public function get connected(): Boolean { return _xiffconn.isActive(); }
		public function get loggedIn(): Boolean { return _xiffconn.isLoggedIn(); }
		
		public function get useParentalControls(): Boolean { return _useParentalControls; }
		public function set useParentalControls(b: Boolean): void { _useParentalControls = b; }
		
		public function connect(): Boolean 
		{ 
			if (realm == null) 
			{  
				// should really set based on what comes back
				
				if (server == "im.comcast.net")
				realm = "comcast.net";
				else
				realm = server;
			}
			
			if ((realm == "comcast.net") && useParentalControls)
			{
				// check parental controls service
				// do as POST to HTTPService, it's easier
				var ws: HTTPService = new HTTPService();
				ws.url = "https://digitalvoice.comcast.net/Comcast/axis2/services/CMService";
				ws.method = "POST";
				ws.headers['SOAPAction'] = '"urn:isCmAuthorized"';
				ws.contentType = "text/xml; charset=utf-8";
				ws.resultFormat = "e4x";
				ws.addEventListener(ResultEvent.RESULT, wsResult);
				ws.addEventListener(FaultEvent.FAULT, wsFault);
				
				var tokenId: String = "SecurityToken-" + UIDUtil.createUID();
				var x:String =
				'<?xml version="1.0" encoding="utf-8"?>'
				+'<soap:Envelope ' 
				+'xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" '
				+'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" '
				+'xmlns:xsd="http://www.w3.org/2001/XMLSchema" '
				+'xmlns:wsa="http://schemas.xmlsoap.org/ws/2004/03/addressing" '
				+'xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd" '
				+'xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">'
				+'<soap:Header>'
				+'<wsse:Security soap:mustUnderstand="1">'
				+'<wsse:UsernameToken '
				+'xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd" ' 
				+'wsu:Id="' + tokenId + '">'
				+'<wsse:Username>' + username + '</wsse:Username>'
				+'<wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText">' + password + '</wsse:Password>'
				+'<wsse:Nonce></wsse:Nonce>'
				+'<wsu:Created></wsu:Created>'
				+'</wsse:UsernameToken>'
				+'</wsse:Security>'
				+'</soap:Header>'
				+'<soap:Body>'
				+'<CmAuthorizedRequest xmlns="http://cpe.comcast.com/cm/types/msg">'
				+'<uid xmlns="">' + username + '</uid>'
				+'</CmAuthorizedRequest>'
				+'</soap:Body>'
				+'</soap:Envelope>';
				
				trace("... connect() checking authentication service");
				
				ws.send(x);
				return true;
			}
			else
			return _xiffconn.connect(tag); 
		}

		private function wsResult(e: ResultEvent):void
		{
			var x: XMLList = e.result.descendants("isAuthorized");
			if ((x.length() > 0) && (x[0].valueOf() == "true"))
			{
				_xiffconn.connect(tag); 
				trace("... authentication service returned true");
			}
			else
			{
				var err: XML =
				<error code="500" condition="authorization failed" type="fault"> failed </error>;
				trace("... authentication service returned ", err.code + "|" + err.condition + "|" + err.message + "|" + err.type);
				this.dispatchEvent(new XMLEvent("error", e.bubbles, e.cancelable, err));
			}
		}

		private function wsFault(e: FaultEvent):void
		{
			var err: XML =
			<error code="500" condition="authorization failed" type="fault"> failed </error>;
			trace("... authentication service fault Error: ", err.code + "|" + err.condition + "|" + err.message + "|" + err.type);
			this.dispatchEvent(new XMLEvent("error", e.bubbles, e.cancelable, err));
		}

		public function disconnect(): void { _xiffconn.disconnect(); }

		public function send(to: String, body: String, formatted: String = null): void
		{
			var msg: Message = new Message();
			if (to.indexOf("@") != -1)
			msg.to = new JID(to);
			else
			msg.to = new JID(to + "@" + realm);
			msg.body = body;
			
			if (formatted != null)
			msg.htmlBody = formatted;

			connection.send(msg);			
		}

		[Bindable(event="propertyChange")]
		public function presence(status: String, show: String = null, priority:int = 5): void
		{
			roster.setPresence(show, status, priority);			
		}		
		
		public function addContact(jid: String, displayName: String, groupName: String): void
		{
			roster.addContact(JID(jid), displayName, groupName);
		}
		
		public function removeContact(jid: String): void
		{
			roster.removeContact(RosterItemVO.get(JID(jid), false));
		}

		public function grantSubscription(jid: String): void
		{
			roster.grantSubscription(JID(jid));
		}
		
		public function denySubscription(jid: String): void
		{
			roster.denySubscription(JID(jid));
		}
		
		private function get connection(): XMPPConnection { return _xiffconn; }

		private function get roster(): Roster 
		{
			// lazy roster
			if (_xiffroster == null)
			_xiffroster = new Roster(_xiffconn); 
			return _xiffroster; 
		}
		
		private var _useParentalControls: Boolean = true;

		private var _xiffconn: XMPPConnection = new XMPPConnection();

		[Bindable]
		private var _xiffroster: Roster = null; //new Roster(_xiffconn);
		
		[Bindable]
		public var rosterTree: XML = new XML("<rosterTree/>");

		public static const offlineGroupText:String = "-Offline-";
		public static const onlineGroupText:String = "-Online-";
		public static const allGroupText:String = "-All-";
		
		private function insertChildDisplayNameSorted(tree: XML, addme: XML):void
		{
			var inserted: Boolean = false;
			
			for each (var x: XML in tree.children())
			{
				if (addme.@displayName <= x.@displayName)
				{
					tree.insertChildBefore(x, addme);
					inserted = true;
					break;
				}
			}
			if (!inserted)
			tree.appendChild(addme);
		}
		
		private function updateRosterTree(eventObj: CollectionEvent):void{
			trace("... updateRosterTree " + eventObj.kind + " location " + eventObj.location + " oldLocation " + eventObj.oldLocation);
			
			// kind may be add, remove, update, or reset
			
			var i: Object;
			var g: String;
			var x: XML;
			var jid: String;
			
			if (eventObj.kind == "reset")
			{
				rosterTree = new XML("<rosterTree/>");
				rosterTree.appendChild(<group displayName={allGroupText}/>);
				rosterTree.appendChild(<group displayName={offlineGroupText}/>);
			}
			else			
			// cheesy way to handle updates for now, by re-adding after deleting
			if ((eventObj.kind == "add") || (eventObj.kind == "update") || (eventObj.kind == "remove"))
			{
				for each (i in eventObj.items)
				{
					if (eventObj.kind == "update")
					i = i.source;
					
					jid = i.jid;
					if (jid == null)
					jid = i.displayName;

					if ((eventObj.kind == "remove") || (eventObj.kind == "update"))
					{
						
						for each (x in rosterTree.group.(@displayName != null).contact.(@jid == jid))
						{
							var y: XML = x.parent();
							if ((y.child("*").length() == 1)
								&& (y.@displayName != allGroupText)
								&& (y.@displayName != offlineGroupText)
								&& (y.@displayName != onlineGroupText))
							delete y.parent().group[y.childIndex()]; 
							else
						 	delete y.contact[x.childIndex()];
						}
					}
					
					if ((eventObj.kind == "add") || (eventObj.kind == "update"))
					{
						insertChildDisplayNameSorted(rosterTree.group.(@displayName == allGroupText)[0], <contact jid={jid} displayName={i.displayName} show={i.show} status={i.status} />);
						
						if (i.show == "unavailable")
						{	
							insertChildDisplayNameSorted(rosterTree.group.(@displayName == offlineGroupText)[0], <contact jid={jid} displayName={i.displayName} show={i.show} status={i.status} />);
						}
						else
						{
							var group:RosterGroup;
							for each (group in roster.getContainingGroups(RosterItemVO(i)))
							{
								g = group.label;
								
								if (rosterTree.group.(@displayName == g).length())
								{
								}
								else
								{
									insertChildDisplayNameSorted(rosterTree, <group displayName={g}/>);
								}
								var grp:XML = rosterTree.group.(@displayName == g)[0];
								insertChildDisplayNameSorted(grp, <contact jid={jid} displayName={i.displayName} show={i.show} status={i.status} />);
							}
						}
					}
				}
			}
			else
			if (eventObj.kind == "update") // at some point we can treat updates better
			{
				for each (i in eventObj.items)
				{
					jid = i.source.jid;
					if (jid == null)
					jid = i.displayName;
					
					///////// TBD: do something!!!
					//
					// which properties do we care about?
					// - displayName
					// - show
					// - status
					// - group
					//
					// do XPATH based replacement
				}
			}
			
			//			trace("--> ", rosterTree.toXMLString());
			return;
		}
		
		[Bindable]
		private var _realm: String;
		
		[Bindable]
		private var _tag: String;

		[Bindable]
		public var xferLog: XML = null; //new XML("<xferLog/>");

		private var timer: Timer = new Timer(10000);		
		
		public function Connection() {
			super();
			
			server = "im.comcast.net";
			realm = null;
			tag = "terminatedFlash";			
			
			timer.addEventListener(TimerEvent.TIMER, onLoginTimeout);
			
			rosterTree.appendChild(<group displayName={allGroupText}/>);
			rosterTree.appendChild(<group displayName={offlineGroupText}/>);
			
			_xiffconn.addEventListener( ConnectionSuccessEvent.CONNECT_SUCCESS, handleConnectionSuccess );
			_xiffconn.addEventListener( DisconnectionEvent.DISCONNECT, handleDisconnection );
			_xiffconn.addEventListener( IncomingDataEvent.INCOMING_DATA, handleIncomingData );
			// since the IQ handler doesn't seem to work, most is done off this call
			
			// doesn't seem to work... xiff.addEventListener( "iq", handleIQ );

			_xiffconn.addEventListener( LoginEvent.LOGIN, handleLogin );

			_xiffconn.addEventListener( OutgoingDataEvent.OUTGOING_DATA, handleOutgoingData );			
			_xiffconn.addEventListener( PresenceEvent.PRESENCE, handlePresence ); 
			// patched XIFF -- otherwise, XIFF throws a null object error here if a Roster is created

			_xiffconn.addEventListener( XIFFErrorEvent.XIFF_ERROR, handleXIFFError );

			if (_xiffroster != null)
			{
				_xiffroster.addEventListener(RosterEvent.USER_AVAILABLE, handleRoster);
				_xiffroster.addEventListener(RosterEvent.USER_UNAVAILABLE, handleRoster);
				
				_xiffroster.addEventListener(RosterEvent.SUBSCRIPTION_DENIAL, handleRoster);
				_xiffroster.addEventListener(RosterEvent.SUBSCRIPTION_REQUEST, handleRoster);
				_xiffroster.addEventListener(RosterEvent.SUBSCRIPTION_REVOCATION, handleRoster);
				
				_xiffroster.addEventListener(CollectionEvent.COLLECTION_CHANGE, updateRosterTree);
				
				_xiffroster.enableAutoUpdate();
			}
		}

		private function onLoginTimeout(eventObj: TimerEvent): void {
			timer.stop();
			if (_xiffconn.isActive() && !_xiffconn.isLoggedIn())
			{
				trace("??? onLoginTimeout: active but not logged in, disconnecting"); 		
				_xiffconn.disconnect();
			}
		}		
		
		private function handleConnectionSuccess(eventObj:ConnectionSuccessEvent): void {
			trace("... Connect: ", eventObj.toString()); 		

			timer.start();
			
			this.dispatchEvent(new Event("connect", eventObj.bubbles, eventObj.cancelable));
		}
		
		private function handleDisconnection(eventObj:DisconnectionEvent): void {
			trace("... Disconnect: ", eventObj.toString());			

			timer.stop();
			
			this.dispatchEvent(new Event("disconnect", eventObj.bubbles, eventObj.cancelable));
		}

		private function handleIncomingData(eventObj:IncomingDataEvent): void {
			trace("... Incoming: ", (eventObj as Object).data);			
			
			var xmlData: XML = new XML(eventObj.data);

			if (xferLog)
			try
			{
				var frm: String = "server";
				if (xmlData.attribute("from").length() > 0)
				frm = xmlData.@from.split("/")[0];
				if (!xferLog.thread.(@contact == frm).child("*").length())
				xferLog.appendChild( <thread contact={frm} /> );
				xferLog.thread.(@contact == frm)[0].appendChild(new XML(eventObj.data));
			}
			catch (e:*)
			{
				trace("??? error adding to xfer log");
			}
			
			this.dispatchEvent(new XMLEvent("incoming", eventObj.bubbles, eventObj.cancelable, xmlData));
			
			// distinguish between iq and message

			// to avoid namespace issues, get localName of element if any
			
			var elem:XML = null;
			var elemLocalName: String;
			
			if (xmlData.elements("*").length() > 0)
			{
				elem = xmlData.elements("*")[0];
				elemLocalName = elem.localName();
			}			
			
			switch (xmlData.localName()) 
			{					
				case "iq":
				if (elemLocalName == "jingle")
				{
					this.dispatchEvent(new XMLEvent("jingle", eventObj.bubbles, eventObj.cancelable, xmlData));					
				}
				break;

				case "message":
				if (xmlData.hasOwnProperty("CInfo")) {
					var decrypted:ByteArray = CM_IM_Decrypt.DecryptCInfoForUser(xmlData.CInfo, username);					
					var plainmsg:XML = new XML(decrypted);
					this.dispatchEvent(new XMLEvent("message", eventObj.bubbles, eventObj.cancelable, <decrypted from={xmlData.@from}>{plainmsg}</decrypted>));
				}
				else
				if (xmlData.hasOwnProperty("body")) {
					this.dispatchEvent(new XMLEvent("message", eventObj.bubbles, eventObj.cancelable, xmlData));
				}
				break;
			}
		}

		private function handleLogin(eventObj:LoginEvent): void {
			trace("... Login: ", eventObj.toString()); 

			timer.stop();
			
			this.dispatchEvent(new Event("login", eventObj.bubbles, eventObj.cancelable));
		}

		private function handleOutgoingData(eventObj:OutgoingDataEvent): void {
			trace("... Outgoing: ", (eventObj as Object).data);

			if (xferLog)
			try {
				var xmlData: XML = new XML(eventObj.data);
				
				var frm: String = "server";
				if (xmlData.attribute("to").length() > 0)
				frm = xmlData.@to.split("/")[0];
				if (!xferLog.thread.(@contact == frm).child("*").length())
				xferLog.appendChild( <thread contact={frm} /> );
				xferLog.thread.(@contact == frm)[0].appendChild(new XML(eventObj.data));
			}
			catch (e:*)
			{
				// last bit of stream is NOT well-formed
				trace("??? error adding to xfer log");
			}
			
			this.dispatchEvent(new XMLEvent("outgoing", eventObj.bubbles, eventObj.cancelable, eventObj.data));
		}

		private function handlePresence(eventObj:PresenceEvent): void {
			var p: XML = <{eventObj.type} id={eventObj.data.id} type={eventObj.data.type}/>;
			if (eventObj.data.from) p.@from = eventObj.data.from;
			if (eventObj.data.to) p.@to = eventObj.data.to;
			if (eventObj.data.show) p.@show = eventObj.data.show;
			if (eventObj.data.status) p.@status = eventObj.data.status; 
			p.@priority = eventObj.data.priority; 
			// may miss extensions -- but those also come in on the presence message to incoming handler
			trace("... Presence: ", p.toXMLString());
			this.dispatchEvent(new XMLEvent("presence", eventObj.bubbles, eventObj.cancelable, p));
		}

		// there are many of these
		private function handleRoster(e:RosterEvent): void {
			var r: XML =
			<roster type={e.type} from={e.jid} />;
			if (e.type == RosterEvent.USER_UNAVAILABLE) 
			{
				// in case we get no data
				r.@type = "unavailable"; 
				r.@show = "unavailable";
			}
			
			var suppress: Boolean = false;
			if (e.data != undefined)
			{
				var presence: String = roster.getPresence(e.jid.bareJid);
				if (presence != null)
				if (presence == e.data.type)
				suppress = true;
				
				r.@type = e.data.type;
				r.@id = e.data.id;
				if (e.data.show) r.@show = e.data.show;
				if (e.data.status) r.@status = e.data.status; 
				if (e.data.priority) r.@priority = e.data.priority;
				// may miss extensions -- but those also come in on the presence message to incoming handler
			}
			
			if (suppress)
			{
				trace("... Roster: suppressing", r.toXMLString());
			}
			else
			{
				trace("... Roster: ", r.toXMLString());
				this.dispatchEvent(new XMLEvent("roster", e.bubbles, e.cancelable, r));
			}
		}
		
		private function handleXIFFError(e:XIFFErrorEvent): void {
			var err: XML =
			<error code={e.errorCode} condition={e.errorCondition} type={e.errorType}> {e.errorMessage} </error>;
			trace("... Error: ", e.errorCode + "|" + e.errorCondition + "|" + e.errorMessage + "|" + e.errorType);
			this.dispatchEvent(new XMLEvent("error", e.bubbles, e.cancelable, err));
		}				
	}
}