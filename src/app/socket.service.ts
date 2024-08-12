// socket.service.ts
import { Injectable } from '@angular/core';
import { io, Socket } from 'socket.io-client';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class SocketService {
  private socket: Socket;

  constructor() {
    this.socket = io('http://127.0.0.1:5000');
    this.socket.on('connect', () => {
      console.log('Connected to server');
    });
  }

  getSocket(): Socket {
    return this.socket;
  }

  joinRoom(email: string) {
    this.socket.emit('join_room', { email });
  }

  leaveRoom(email: string) {
    this.socket.emit('leave_room', { email });
  }

  sendMessage(data: { sender_email: string, recipient_email: string, message: string }) {
    this.socket.emit('send_message', data);
  }

  receiveMessage(): Observable<any> {
    return new Observable(observer => {
      this.socket.on('receive_message', (message) => {
        observer.next(message);
      });
    });
  }

  getRoomAnnouncements(): Observable<any> {
    return new Observable(observer => {
      this.socket.on('join_room_announcement', (message) => {
        observer.next(message);
      });

      this.socket.on('leave_room_announcement', (message) => {
        observer.next(message);
      });
    });
  }
}
