package com.app.meals.ui.components

import androidx.compose.animation.animateContentSize
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.LazyVerticalGrid
import androidx.compose.foundation.lazy.grid.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ExpandLess
import androidx.compose.material.icons.filled.ExpandMore
import androidx.compose.material.icons.filled.Warning
import androidx.compose.material3.Button
import androidx.compose.material3.Card
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.CircularProgressIndicator
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.material3.pulltorefresh.PullToRefreshBox
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.dimensionResource
import androidx.compose.ui.res.stringResource
import coil.compose.AsyncImage
import coil.request.ImageRequest
import com.app.meals.R
import com.app.meals.model.Meal

@Composable
fun MealsList(
    meals: List<Meal>,
    isRefreshing: Boolean,
    onRefresh: () -> Unit,
    onMealClick: (String) -> Unit,
    modifier: Modifier = Modifier,
    contentPadding: PaddingValues = PaddingValues(dimensionResource(R.dimen.dimen_0))
) {
    PullToRefreshBox(
        isRefreshing = isRefreshing,
        onRefresh = onRefresh,
        modifier = modifier.fillMaxSize()
    ) {
        // Instead of LazyColumn, we use LazyVerticalGrid for 2x2 grid
        LazyVerticalGrid(
            columns = GridCells.Adaptive(minSize = dimensionResource(R.dimen.dimen_300)),
            modifier = modifier.fillMaxSize().padding(horizontal = dimensionResource(R.dimen.dimen_16)),
            contentPadding = contentPadding,
            verticalArrangement = Arrangement.spacedBy(dimensionResource(R.dimen.dimen_16)),
            horizontalArrangement = Arrangement.spacedBy(dimensionResource(R.dimen.dimen_16))
        ) {
            items(items = meals, key = { meal -> meal.id }) { meal ->
                ExpandableMealCard(
                    meal = meal,
                    onCardClick = { onMealClick(meal.id) }
                )
            }
        }
    }
}

/**
 * Reusable expandable card component for displaying a meal item.
 */
@Composable
fun ExpandableMealCard(
    meal: Meal,
    onCardClick: () -> Unit,
    modifier: Modifier = Modifier
) {
    var expanded by remember { mutableStateOf(false) }

    Card(
        modifier = modifier
            .fillMaxWidth()
            .clickable { onCardClick() },
        shape = MaterialTheme.shapes.large,
        colors = CardDefaults.cardColors(
            containerColor = MaterialTheme.colorScheme.surface
        ),
        border = BorderStroke(dimensionResource(R.dimen.dimen_1), MaterialTheme.colorScheme.outlineVariant)
    ) {
        Column(modifier = Modifier.animateContentSize()) {
            // Modularized Image Top Section
            MealCardImage(imageUrl = meal.imageUrl, mealName = meal.name)

            // Modularized Main Row (Title + Arrow)
            MealCardHeader(
                mealName = meal.name,
                expanded = expanded,
                onExpandClick = { expanded = !expanded }
            )

            // Modularized Expanded Quick Details Section
            if (expanded) {
                MealCardDetails(meal = meal)
            }
        }
    }
}

/**
 * Top section of the card displaying the cropped meal image.
 */
@Composable
fun MealCardImage(imageUrl: String?, mealName: String) {
    Box {
        AsyncImage(
            model = ImageRequest.Builder(context = LocalContext.current)
                .data(imageUrl)
                .crossfade(true)
                .build(),
            contentDescription = mealName,
            contentScale = ContentScale.Crop,
            modifier = Modifier
                .fillMaxWidth()
                .height(dimensionResource(R.dimen.dimen_220))
                .clip(MaterialTheme.shapes.large)
        )
    }
}

/**
 * Main header inside the card with the meal title and the toggle arrow icon.
 */
@Composable
fun MealCardHeader(
    mealName: String,
    expanded: Boolean,
    onExpandClick: () -> Unit
) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = dimensionResource(R.dimen.dimen_16), vertical = dimensionResource(R.dimen.dimen_12)),
        verticalAlignment = Alignment.CenterVertically
    ) {
        Text(
            text = mealName,
            style = MaterialTheme.typography.titleLarge,
            color = MaterialTheme.colorScheme.onSurface,
            modifier = Modifier.weight(1f)
        )
        IconButton(onClick = onExpandClick) {
            Icon(
                imageVector = if (expanded) Icons.Filled.ExpandLess else Icons.Filled.ExpandMore,
                contentDescription = if (expanded) stringResource(R.string.show_less) else stringResource(R.string.show_more),
                tint = MaterialTheme.colorScheme.primary
            )
        }
    }
}

/**
 * Expanded body area showing dynamic quick facts about the recipe.
 */
@Composable
fun MealCardDetails(meal: Meal) {
    val originTags = listOfNotNull(meal.category, meal.country).joinToString(stringResource(R.string.join_to_string))
    val ingredientsCount = meal.ingredients.size

    Column(
        modifier = Modifier
            .fillMaxWidth()
            .padding(start = dimensionResource(R.dimen.dimen_16), end = dimensionResource(R.dimen.dimen_16), bottom = dimensionResource(R.dimen.dimen_16))
    ) {
        if (originTags.isNotEmpty()) {
            Text(
                text = originTags,
                style = MaterialTheme.typography.bodyMedium,
                color = MaterialTheme.colorScheme.primary,
                modifier = Modifier.padding(bottom = dimensionResource(R.dimen.dimen_4))
            )
        }

        if (ingredientsCount > 0) {
            Text(
                text = stringResource(R.string.required_ingredients, ingredientsCount),
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant
            )
        } else {
            Text(
                text = stringResource(R.string.full_info),
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant
            )
        }
    }
}

/**
 * The home screen displaying the loading message.
 */
@Composable
fun LoadingScreen(modifier: Modifier = Modifier) {
    Box(
        contentAlignment = Alignment.Center,
        modifier = modifier.fillMaxSize()
    ) {
        CircularProgressIndicator(
            modifier = Modifier.size(dimensionResource(R.dimen.dimen_64)),
            color = MaterialTheme.colorScheme.primary,
            trackColor = MaterialTheme.colorScheme.surfaceVariant,
        )
    }
}

/**
 * The home screen displaying error message with re-attempt button.
 */
@Composable
fun ErrorScreen(retryAction: () -> Unit, modifier: Modifier = Modifier) {
    Column(
        modifier = modifier.fillMaxSize(),
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally
    ) {
        Icon(
            imageVector = Icons.Default.Warning,
            contentDescription = null,
            modifier = Modifier.size(dimensionResource(R.dimen.dimen_64)),
            tint = MaterialTheme.colorScheme.error
        )
        Text(
            text = stringResource(R.string.failed_to_load),
            modifier = Modifier.padding(dimensionResource(R.dimen.dimen_16)),
            style = MaterialTheme.typography.bodyLarge
        )
        Button(onClick = retryAction) {
            Text(stringResource(R.string.retry))
        }
    }
}