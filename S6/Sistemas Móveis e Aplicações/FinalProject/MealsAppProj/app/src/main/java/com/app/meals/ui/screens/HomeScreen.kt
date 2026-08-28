package com.app.meals.ui.screens

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.dimensionResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import com.app.meals.R
import com.app.meals.ui.components.ErrorScreen
import com.app.meals.ui.components.LoadingScreen
import com.app.meals.ui.components.MealsList
import com.app.meals.ui.viewModels.HomeUiState

@Composable
fun HomeScreen(
    homeUiState: HomeUiState,
    onRefresh: () -> Unit,
    retryAction: () -> Unit,
    onMealClick: (String) -> Unit,
    modifier: Modifier = Modifier,
    contentPadding: PaddingValues = PaddingValues(dimensionResource(R.dimen.dimen_0)),
) {
    Column(
        modifier = modifier
            .padding(contentPadding)
    ) {
        Text(
            text = stringResource(R.string.popular),
            fontWeight = FontWeight.Bold,
            style = MaterialTheme.typography.titleLarge,
            modifier = Modifier.padding(dimensionResource(R.dimen.dimen_16)),
            color = MaterialTheme.colorScheme.onSurface
        )
        Box(modifier = Modifier.weight(1f)) {
            when(homeUiState){
                is HomeUiState.Loading -> LoadingScreen(modifier = Modifier.fillMaxSize())
                is HomeUiState.Success -> MealsList(
                    homeUiState.meals,
                    isRefreshing = false,
                    onRefresh = onRefresh,
                    onMealClick = onMealClick,
                    contentPadding = PaddingValues(bottom = dimensionResource(R.dimen.dimen_16)),
                    modifier = Modifier.fillMaxSize()
                )
                is HomeUiState.Error -> ErrorScreen(retryAction, modifier = Modifier.fillMaxSize())
            }
        }
    }
}