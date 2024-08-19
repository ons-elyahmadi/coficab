 // src/app/auth.guard.ts
import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class AuthGuard implements CanActivate {
  constructor(private router: Router) {}

  canActivate(): Observable<boolean> | Promise<boolean> | boolean {
    const token = localStorage.getItem('token');
    if (token) {
      return true; // Token exists, allow access
    } else {
      this.router.navigate(['/login']); // Redirect to login if no token
      return false;
    }
  }
}
