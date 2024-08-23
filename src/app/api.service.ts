import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, throwError } from 'rxjs';
import { map } from 'rxjs/operators';
export interface TableData {
  [key: string]: any;
}

@Injectable({
  providedIn: 'root'
})
export class ApiService {
   
  private baseUrl = 'http://127.0.0.1:5000/api';    // Update with your Flask server address
  private token: string | null = localStorage.getItem('token'); // Initialize with stored token

  constructor(private http: HttpClient) {}
 
  

   
  login(email: string, password: string): Observable<any> {
    return this.http.post<any>(`${this.baseUrl}/login`, { email, password })
      .pipe(
        map(response => {
          this.token = response.token;  // Store JWT token
          if (this.token) {
            localStorage.setItem('token', this.token); // Store token in localStorage
          }
          return response;
        })
      );
  }

  private getHeaders(): HttpHeaders {
    if (!this.token) {
      throw new Error('No token available');
    }
    return new HttpHeaders({
      'Authorization': `Bearer ${this.token}`
    });
  }

  clearToken(): void {
    this.token = null;
  }

  // Example API method that requires authorization
  sendMessage(data: any): Observable<any> {
    return this.http.post<any>(`${this.baseUrl}/send_message`, data, {
      headers: this.getHeaders()
    });
  }

  logout(): Observable<any> {
    const token = localStorage.getItem('token'); // Retrieve token
    if (!token) {
      return throwError('No token found'); // Handle case where token is not available
    }
    return this.http.post<any>(`${this.baseUrl}/logout`, {}, {
      headers: new HttpHeaders({
        'Authorization': `Bearer ${token}`  // Send token in header
      })
    }).pipe(
      map(response => {
        this.clearToken(); // Clear token after successful logout
        localStorage.removeItem('token'); // Remove token from localStorage
        return response;
      })
    );
  }
  

   
 
 
  runRScript(): Observable<any> {
    return this.http.post<any>(`${this.baseUrl}/run-r-script`, {});
  }
  runRCScript(): Observable<any> {
    return this.http.post<any>(`${this.baseUrl}/run-rc-script`, {});
  }
  runRfScript(): Observable<any> {
    return this.http.post<any>(`${this.baseUrl}/run-rf-script`, {});
  }
  runSSIR(): Observable<any> {
    return this.http.post<any>(`${this.baseUrl}/run-ssis`, {});
  }
  getMessages(sender_email: string, recipient_email: string): Observable<any[]> {
    return this.http.get<any[]>(`${this.baseUrl}/messages`, {
      params: { sender_email, recipient_email }
    });
  }
   
  getUserRole(): string | null {
    return localStorage.getItem('userRole');
  }
   

  getTables(): Observable<string[]> {
    return this.http.get<string[]>(`${this.baseUrl}/tables`);
  }

  sendTableCsv(data: { table_name: string, recipient: string, subject?: string }): Observable<any> {
    return this.http.post<any>(`${this.baseUrl}/send_table_csv`, data);
  }
   
   
  signup(email: string, password: string, ROLE: string, username: string): Observable<any> {
    return this.http.post<any>(`${this.baseUrl}/signup`, { email, password, ROLE, username });
  }
  getUsers(): Observable<any> {
    return this.http.get<any>(`${this.baseUrl}/users`  , { headers: this.getHeaders() });
  }
  sendMail(recipient: string, subject: string, body: string): Observable<any> {
    return this.http.post<any>(`${this.baseUrl}/sendemail`, { recipient, subject, body });
  }
   
  getSentEmails(): Observable<any[]> {
    return this.http.get<any[]>(`${this.baseUrl}/emails`);
  }

  deleteSentEmail(emailId: string): Observable<any> {
    return this.http.delete<any>(`${this.baseUrl}/emails/${emailId}`);
  }
  updateUser(id: number, email: string, password: string, role: string, username: string): Observable<any> {
    return this.http.put<any>(`${this.baseUrl}/users/${id}`, { email, password, ROLE: role, username });
  }
  
  deleteUser(id: number): Observable<any> {
    return this.http.delete<any>(`${this.baseUrl}/users/${id}`);
  }
   

  getTableData(tableName: string): Observable<any[]> {
    return this.http.get<any[]>(`${this.baseUrl}/table-data/${tableName}`);
  }
   
  
}
 