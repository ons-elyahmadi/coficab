import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { ApiService } from './api.service';

@Injectable({
  providedIn: 'root'
})
export class AuthService {
  private currentUserEmail: string = '';

  constructor(private router: Router, private apiService: ApiService) {}

  setCurrentUser(email: string): void {
    this.currentUserEmail = email;
  }

  getCurrentUser(): string {
    return this.currentUserEmail;
  }

  isLoggedIn(): boolean {
    return !!localStorage.getItem('currentUser');
  }

  hasRole(role: string): boolean {
    const storedRole = localStorage.getItem('ROLE');
    return storedRole === role;
  }

  logout(): void {
    localStorage.removeItem('currentUser');
    localStorage.removeItem('ROLE');
    localStorage.removeItem('token'); // Also clear the stored token
    this.apiService.clearToken(); // Clear the token in the ApiService
    this.router.navigate(['/login']);
  }
}
