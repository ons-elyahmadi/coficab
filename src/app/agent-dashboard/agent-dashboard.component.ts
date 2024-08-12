import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-agent-dashboard',
  templateUrl: './agent-dashboard.component.html',
  styleUrls: ['./agent-dashboard.component.css']
})
export class AgentDashboardComponent {
  constructor(private router: Router) { }

  // Méthodes pour la gestion des utilisateurs, envoi de mails, etc.
}

