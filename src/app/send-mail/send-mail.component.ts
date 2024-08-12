 import { Component } from '@angular/core';
import { ApiService } from '../api.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  selector: 'app-send-mail',
  templateUrl: './send-mail.component.html',
  styleUrls: ['./send-mail.component.css']
})
export class SendMailComponent {
  recipient: string = '';
  subject: string = '';
  body: string = '';
  successMessage: string = '';
  errorMessage: string = '';

  constructor(private apiService: ApiService, private _snackBar: MatSnackBar) { }

  sendMail(): void {
    this.apiService.sendMail(this.recipient, this.subject, this.body).subscribe(
      response => {
        this.successMessage = 'Email sent successfully';
        this.errorMessage = '';
        this.showAlert(this.successMessage, 'success');
        this.clearForm();
      },
      error => {
        this.errorMessage = 'Failed to send email';
        this.successMessage = '';
        this.showAlert(this.errorMessage, 'error');
      }
    );
  }

  showAlert(message: string, type: string): void {
    this._snackBar.open(message, '', {
      duration: 3000,
      panelClass: type === 'success' ? 'snack-bar-success' : 'snack-bar-error'
    });
  }

  clearForm(): void {
    this.recipient = '';
    this.subject = '';
    this.body = '';
  }
}
