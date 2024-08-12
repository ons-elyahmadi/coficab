import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ApiService } from '../api.service';  // Assurez-vous que le chemin est correct
import { Router } from '@angular/router';

@Component({
  selector: 'app-signup',
  templateUrl: './signup.component.html',
  styleUrls: ['./signup.component.css']
})
export class SignupComponent implements OnInit {
  signupForm: FormGroup;
  errorMessage: string = '';

  constructor(
    private fb: FormBuilder,
    private apiService: ApiService,
    private router: Router
  ) {
    this.signupForm = this.fb.group({
      email: ['', [Validators.required, Validators.email]],
      password: ['', [Validators.required, Validators.minLength(6)]],
      username: ['', Validators.required],
      role: ['', Validators.required]
    });
  }

  ngOnInit(): void { }

  signup() {
    if (this.signupForm.invalid) {
      return;
    }

    const { email, password, username, role } = this.signupForm.value;

    this.apiService.signup(email, password, role, username).subscribe(
      (response: any) => {
        console.log('User signed up successfully', response);
        this.router.navigate(['/login']);
      },
      (error: any) => {
        console.error('Error signing up', error);
        this.errorMessage = error.error.message || 'An error occurred. Please try again.';
      }
    );
  }
}
