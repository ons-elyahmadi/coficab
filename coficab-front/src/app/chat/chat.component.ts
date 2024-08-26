import { Component, OnInit, OnDestroy } from '@angular/core';
import { SocketService } from '../socket.service';
import { ApiService } from '../api.service';
import { AuthService } from '../auth.service';

import { MatSnackBar } from '@angular/material/snack-bar';
@Component({
  selector: 'app-chat',
  templateUrl: './chat.component.html',
  styleUrls: ['./chat.component.css']
})
export class ChatComponent implements OnInit, OnDestroy {
  message: string = '';
  messages: any[] = [];
  users: any[] = []; // Mettez à jour avec votre modèle utilisateur
  recipientEmail: string = '';
  email: string = '';

  constructor(private socketService: SocketService,    private snackBar: MatSnackBar, private apiService: ApiService, private authService: AuthService) { }

  ngOnInit(): void {
    this.email = this.authService.getCurrentUser();
   
    this.apiService.getUsers().subscribe(users => {
      this.users = users;
    });
  
    this.socketService.receiveMessage().subscribe((data: any) => {
      if ((data.sender_email === this.email && data.recipient_email === this.recipientEmail) ||
          (data.sender_email === this.recipientEmail && data.recipient_email === this.email)) {
        this.messages.push(data);
        this.showNotification('New message received');
         
      }
    });
  
    this.socketService.getRoomAnnouncements().subscribe((data: any) => {
      console.log(data.message);
    });
  
    this.socketService.joinRoom(this.email);
  
  }

  ngOnDestroy(): void {
    this.socketService.leaveRoom(this.email);
  }

  sendMessage(): void {
    if (this.message && this.message.trim() !== '' && this.recipientEmail) {
      const messageData = {
        sender_email: this.email,
        recipient_email: this.recipientEmail,
        message: this.message,
        timestamp: new Date()
      };
      this.socketService.sendMessage(messageData);
       
      this.message = '';
      this.messages.push(messageData);
       
       
    }
     
  }

  loadMessages(): void {
    if (this.recipientEmail) {
      this.apiService.getMessages(this.email, this.recipientEmail).subscribe((messages: any[]) => {
        this.messages = messages;
        this.messages.sort((a, b) => new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime());
      });
    }
    
  }
  showNotification(message: string): void {
    this.snackBar.open(message, 'Close', {
      duration: 3000,
    });
  }
  
}
