import { Component, OnInit } from '@angular/core';
import { ApiService } from '../api.service';
import { Router, NavigationEnd } from '@angular/router';
import { AuthService } from '../auth.service'; 
import { filter } from 'rxjs/operators';

@Component({
  selector: 'app-agent-dashboard',
  templateUrl: './agent-dashboard.component.html',
  styleUrls: ['./agent-dashboard.component.css']
})
export class AgentDashboardComponent implements OnInit {
  showVideo: boolean = false; // Initialize with false to hide video by default

  constructor(
    private apiService: ApiService,
    private router: Router,
    private authService: AuthService // Inject AuthService
  ) {}

  ngOnInit(): void {
    // Check the current URL on component initialization
    this.checkVideoDisplay();

    // Update showVideo whenever navigation ends
    this.router.events.pipe(
      filter(event => event instanceof NavigationEnd)
    ).subscribe(() => {
      this.checkVideoDisplay();
    });
  }

  checkVideoDisplay() {
    // Show video only when URL is '/agent-dashboard'
    this.showVideo = this.router.url === '/agent-dashboard';
  }

  logout() {
    this.apiService.logout().subscribe(
      response => {
        console.log('Logout successful:', response);
        this.authService.logout(); // Use AuthService to handle logout logic
        this.router.navigate(['/login']); // Redirect to login page after logout
      },
      error => {
        console.error('Logout failed:', error);
      }
    );
  }
}
