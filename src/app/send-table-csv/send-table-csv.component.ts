import { Component, OnInit } from '@angular/core';
import { ApiService } from '../api.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  selector: 'app-send-table-csv',
  templateUrl: './send-table-csv.component.html',
  styleUrls: ['./send-table-csv.component.css']
})
export class SendTableCsvComponent implements OnInit {
  tables: string[] = [];
  selectedTable: string = '';
  recipient: string = '';
  subject: string = '';
  body: string = '';
  successMessage: string = '';
  errorMessage: string = '';

  constructor(private apiService: ApiService, private _snackBar: MatSnackBar) { }

  ngOnInit(): void {
    this.apiService.getTables().subscribe(
      tables => this.tables = tables,
      error => this.showAlert('Failed to load tables', 'error')
    );
  }

  sendTableCsv(): void {
    if (!this.selectedTable || !this.recipient) {
      this.showAlert('Please select a table and provide recipient email', 'error');
      return;
    }
    this.apiService.sendTableCsv({ table_name: this.selectedTable, recipient: this.recipient, subject: this.subject }).subscribe(
      response => {
        this.successMessage = 'Email sent successfully with the CSV attachment';
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
    this.selectedTable = '';
    this.recipient = '';
    this.subject = '';
    this.body = '';
  }

  onTableChange(): void {
    if (this.selectedTable) {
      this.subject = `Data from ${this.selectedTable}`;
    }
  }

  
}
