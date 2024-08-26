// login.component.ts
import { Component } from '@angular/core';
import { ApiService } from '../api.service';
import { Router } from '@angular/router';
import { AuthService } from '../auth.service';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent {
  email: string = '';
  password: string = '';
  errorMessage: string = '';

  constructor(private apiService: ApiService, private router: Router, private authService: AuthService) { }

  login(): void {
    this.apiService.login(this.email, this.password)
      .subscribe(
        response => {
          console.log(response);
          const ROLE = response.ROLE;
          localStorage.setItem('currentUser', JSON.stringify(response)); // Store user data in localStorage
          localStorage.setItem('ROLE', ROLE); 
          localStorage.setItem('email', this.email);    // Store role separately if needed
          this.authService.setCurrentUser(this.email); // Set current user email

          if (ROLE === 'User') {
            this.router.navigate(['/dashboard']);
          } else if (ROLE === 'Agent') {
            this.router.navigate(['/agent-dashboard']);
          } else {
            this.errorMessage = 'Invalid role';
          }
        },
        error => {
          console.error(error);
          this.errorMessage = error.error.message || 'An error occurred during login';
        }
      );
  }
}
