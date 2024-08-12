// auth.service.ts
import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class AuthService {
  private currentUserEmail: string = '';

  setCurrentUser(email: string): void {
    this.currentUserEmail = email;
  }

  getCurrentUser(): string {
    return this.currentUserEmail;
  }
}
